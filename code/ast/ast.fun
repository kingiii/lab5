functor Ast(): AST =
struct

type pos = int

exception Unimplemented

val tag = false
fun debug s =
    if tag then 
	print (String.concat [s, "\n"])
    else 
	()

fun spaces n =
    case n
     of 0 => ""
      | n => String.concat [" ", spaces (n - 1)]  
	     
structure Binop =
struct
datatype t
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Gt
  | Eq
  | Ge
  | Le
  | Neq
    
val toString =
 fn Plus => " + "
  | Minus => " - "
  | Times => " * "
  | Div => "/"
  | Mod => "%"
  | And => " && "
  | Or => "||"
  | Gt => " > "
  | Lt => " < "
  | Eq => "=="
  | Ge => ">="
  | Le => "<="
  | Neq => "!="
end

structure Tipe =
struct
datatype t
  = Int
  | Bool
  | ArrayInt
  | Name of string
            
fun equals (t1, t2) =
    case (t1, t2)
     of (Int, Int) => true
      | (Bool, Bool) => true
      | (ArrayInt, ArrayInt) => true
      | (Name s1, Name s2) => s1 = s2
      | _ => false
             
fun toString t = 
    case t
     of Int => "int"
      | Bool => "boolean"
      | ArrayInt => "int []"
      | Name s => s
end

structure Exp =
struct
datatype t
  = Inti of int
  | True
  | False
  | Var of {id : string, pointer : string, pos : pos}
  | Binop of t * Binop.t * t * pos
  | NewArray of t * pos
  | NewId of string * pos
  | This of pos
  | Not of t * pos
  | Length of t * pos
  | Call of t * Tipe.t * string * Tipe.t * t list * pos
  | SimpleCall of string * Tipe.t * t list * pos
  | Array of t * t * pos
  | ParenExp of t * pos
		
fun printExps (sp, []) = ""
  | printExps (sp, es) = 
    let val size:int = List.length es
	val e::ex = es
    in
	if size = 1 then printExp(sp, e)
	else 
	    (printExp(sp, e)) ^ ", " ^ (printExps(sp,ex))
    end
and printExp(sp, x):string =
    case x
     of Inti i => Int.toString i
      | True => "true"
      | False => "false"
      | Var {id, pointer, pos} => id
      | Binop (e1, b, e2, pos) => 
        String.concat [printExp(sp, e1), 
                       Binop.toString b, printExp(sp, e2)]
      | NewArray (e, pos) => 
        String.concat ["new int ", printExp(sp, e)]
      | NewId (id, pos) => 
        String.concat ["new ", id, "()"]
      | This pos => "this"
      | Not (e, pos) => 
        String.concat ["!", printExp(sp, e)]
      | Length (e, pos) => 
        String.concat [printExp(sp, e), ".length"]
      | Call (e1, ety, id, retty, es, pos) => 
	let val _ = debug("in call()")
	in
	    String.concat [printExp(sp, e1), ".", id, "(", printExps(sp, es), ")"]
	end
      | SimpleCall (id, retty, es, pos) => 
	let val _ = debug("in simpleCall()")
	in
	    String.concat [id, "(", printExps(sp, es), ")"]
	end
      | Array (e1, e2, pos) => 
        String.concat [printExp(sp, e1), "[", 
                       printExp(sp, e2), "]"]    
      | ParenExp(e, pos) => String.concat["(", printExp(sp,e), ")"]
end

structure Stm =
struct
datatype t
  = AssignId of {id: string, e: Exp.t, 
                 pointer: string, pos: pos}
  | AssignArray of {id: string, left: Exp.t, 
                    right : Exp.t, pointer: string, 
                    pos: pos}
  | If of Exp.t * t list * t list * pos
  | While of Exp.t * t list * pos
  | Return of Exp.t * pos
  | Print of Exp.t * pos
             
fun printStm (sp, x) =
    case x
     of If (e, ss1, ss2, pos) => String.concat [
				 spaces sp, "if ", "(", Exp.printExp(sp, e) , "){\n",
				 printStms (sp + 4, ss1),
				 spaces sp, "}\n",
				 spaces sp, "else{\n",
				 printStms (sp + 4, ss2),
				 spaces sp, "}\n"]
      | While (e, ss, pos) =>
        let val e_str = Exp.printExp (sp, e)
            val stms_str = printStms (sp + 4, ss)
        in
	    String.concat [spaces sp, "while ", "(", e_str, "){\n",
			   stms_str,
			   spaces sp, "}\n"]
        end
      | AssignId {id, e, pointer, pos} =>
	let val _ = debug("in AssignId, " ^ id ^ " = " ^ "\n")
	in
	    if pointer = "" then
		String.concat [spaces sp, id, " = ", Exp.printExp(sp, e), ";\n"]
	    else
		String.concat [spaces sp, pointer, ".", id, " = ", Exp.printExp(sp, e), ";\n"]
	end
      | AssignArray {id, left, right, pointer, pos} => 
	String.concat [spaces sp, id, "[", Exp.printExp(sp, left), "]", " = ", Exp.printExp(sp, right), ";\n"]
      | Return (e, pos) => String.concat [spaces sp, "return (", Exp.printExp(sp, e), ");\n"]
      | Print (e, pos) => 
	let val _ = debug("in Print()")
	in
	    String.concat ["System.out.println (", Exp.printExp(sp, e), ");"]   
	end
and printStms (spaces, f) =
    String.concat (List.map (fn x => printStm (spaces, x)) f)	   

end

structure Method =
struct
datatype t
  = T of {name: string,
          prefix: string,
          rettype: Tipe.t,
          args: {id: string, ty: Tipe.t} list,
          locals: {id: string, ty: Tipe.t} list,
          stms: Stm.t list,              
          pos: pos}
	 
val bogus = T {name = "bogus",
               prefix = "",
               rettype = Tipe.Int,
               args = [],
               locals = [],
               stms = [],
               pos = ~1}

fun compare (T {name = name1, prefix = p1, ...}, 
             T {prefix = p2, name = name2, ...}) =
    String.compare (String.concat [p1, "_", name1], 
                    String.concat [p2, "_", name2])
    
fun equals (T {name = name1, ...}, 
            T {name = name2, ...}) = name1 = name2

fun greater (T {name = name1, ...}, 
             T {name = name2, ...}) = name1 > name2		

fun printVar {id, ty} = String.concat [Tipe.toString ty, " ", id, ";\n"]    

fun printVars vars = String.concat (List.map printVar vars)
		     
fun printArgs args =
    case args
     of [] => ""
      |[{id, ty}] => String.concat [Tipe.toString ty, " ", id] 
      | {id, ty} :: xs =>
        String.concat [Tipe.toString ty, " ", id, ", ", printArgs xs]    			
	
fun printMethod f =
    case f
     of T {name, prefix, rettype, args, locals, stms, pos} =>
        let val s1 = if prefix = ""
                     then String.concat [spaces 4, "public ", Tipe.toString rettype, " ", 
                                         name, " (", printArgs args, ") ", "{\n"]
                     else String.concat [spaces 4, "public ", Tipe.toString rettype, " ", 
                                         prefix, "_", name, " (", printArgs args, ") ", "{\n"]
            val _ = debug("methodName = " ^ name ^ "\n")
	    val s2 = String.concat [spaces 8, printVars locals, "\n"]
	    val _ = debug "printVars finish\n"
            val s3 = Stm.printStms (8, stms)
	    val _ = debug "printStms finish\n"
        in  String.concat [s1, s2, s3, spaces 4, "}\n"]
        end
	
end

structure Class =
struct

datatype t
  = T of {name : string,
          extends : string option,
          cvars : {ty : Tipe.t, id : string, 
                   prefix : string} list,
          methods : Method.t list,
          pos : pos}
	 
fun compare (T {name = n1, ...}, 
             T {name = n2, ...}) = String.compare (n1, n2) 
    
fun equals (T {name = n1, ...}, 
            T {name = n2, ...}) = n1 = n2    
    
fun equalsTyVarPrefix ({ty = ty1, id = id1, prefix = p1}, 
                       {ty = ty2, id = id2, prefix = p2})
    = id1 = id2 andalso Tipe.equals (ty1, ty2)
			
val bogus = T {name = "",
               extends = SOME(""),
               cvars = [],
	       methods = [],
	       pos = ~1}
	    
(*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*)
fun greaterVar( {ty = ty1 : Tipe.t, id = name1 : string, prefix = prefix1 : string},
		{ty = ty2 : Tipe.t, id = name2 : string, prefix = prefix2 : string}) = name1 > name2
    
fun findGreatest ([], func: 'a * 'a -> bool) = []
  |	findGreatest (x1::xn_1, func: 'a * 'a -> bool) =
	if( List.length(xn_1) > 0) then
	    let 
		val x2::xn_2 = xn_1
	    in
		if func(x1, x2) then
		    x2::findGreatest(x1::xn_2, func)
		else
		    x1::findGreatest(xn_1, func)
	    end
	else
	    x1::xn_1

fun sortList ([], func: 'a * 'a -> bool) = []
  | sortList (xs, func: 'a * 'a -> bool) =
    let 
	val newXs = findGreatest(xs, func) 
	val pre_n_1 = List.take(newXs, (List.length(newXs)-1) )
	val sorted_n_1 = sortList(pre_n_1, func)
	val last_1 = List.last(newXs)
    in
	sorted_n_1@[last_1]
    end

fun sortVarList [] = [] 
  | sortVarList cvars =  sortList(cvars, greaterVar)
			 
fun sortMethodList [] = [] 
  | sortMethodList methods =  sortList(methods, Method.greater)

fun sortClass (	T{name = name1: string, extends = extends1: string option, 
		  cvars = cvars1: ({ty : Tipe.t, id : string, prefix : string} list), methods = methods1: Method.t list, pos = pos1: pos} ) =
    let 
	val newCvars: ({ty : Tipe.t, id : string, prefix : string} list) = sortVarList(cvars1)
	val newMethods: Method.t list = sortMethodList(methods1)
	val newClass = T{name = name1, extends = extends1, cvars = newCvars, methods = newMethods, pos = pos1}
    in
	newClass
    end

fun sortClasses [] = []
  | sortClasses (c::cs) =
    let 
	val newc = sortClass(c)
    in
	newc::sortClasses(cs)
    end

(*-------------------------------------------------------------------------------*)
    
fun printClassVar {id, ty, prefix} =
    if prefix = "" then 
	String.concat [spaces 4, Tipe.toString ty, " ", id, ";\n"]
    else 
	String.concat [spaces 4, Tipe.toString ty, " ", prefix, "_", id, ";\n"]
	
fun printClassVars vars = String.concat (List.map printClassVar vars)
			  
fun printClass f =
    case f
     of T {name, extends, cvars, methods, pos} =>
        let val s1 =
                case extends
                 of NONE => String.concat ["class ", name, "{", "\n"]
                  | SOME x => String.concat ["class ", name, " extends ", x, " { ", "\n"]
            val s2 = printClassVars cvars
            val s3 = List.map Method.printMethod methods
            val s4 = String.concat s3
        in  String.concat [s1, s2, s4, "}\n"]
        end

end

structure MainClass =
struct
datatype t
  = T of {name : string,
          arg : string,
          locals: {id: string, ty: Tipe.t} list,
          stms : Stm.t list,
          pos : pos}
         
val bogus = T {name = "@@@", locals = [], arg = "", stms = [], pos = 0} 

fun printMainClass f =
    case f
     of T {name, arg, locals, stms, pos} => 
        let 
	    val s1 = String.concat ["class ", name, "{", "\n", "\tpublic static void main(String[] ", arg, "){\n"]
            val _ = debug("printVars begin in main()\n")
	    val locals_str = String.concat [spaces 8, Method.printVars locals, "\n"]
            val _ = debug("printStm begin in main()\n")
	    val s3 = Stm.printStms (8, stms)
            val _ = debug("printStm finish in main()\n")
        in  if name = "@@@"
            then ""
            else String.concat [s1, locals_str, s3, "\t}\n}\n"]
        end

end

structure Program =
struct
datatype t
  = T of {mainClass: MainClass.t,
          classes: Class.t list,
          pos: pos}
         
val bogus = T {mainClass = MainClass.bogus,
               classes = [],
               pos = ~1}
	    
fun printProg f = 
    case f
     of T {mainClass, classes, pos} =>
        let 
	    val _ = debug("main start!\n")
	    val strMainClass = String.concat [MainClass.printMainClass mainClass, "\n\n\n"]
	    val _ = debug("main finish!\n")
	    val strClasses = String.concat (List.map (fn c => (Class.printClass c)^"\n\n\n") classes)
        in  String.concat [strMainClass, strClasses, "\n"]
        end

(*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*)
fun sortProg f = 
    case f
     of T {mainClass, classes, pos} =>
        let 
            val newClasses = Class.sortClasses(classes) 
        in  
	    T {mainClass = mainClass, classes = newClasses, pos = pos}
        end	
(*-------------------------------------------------------------------------------*)
	
end

val printAST = Program.printProg
val sortAST = Program.sortProg

end
