functor Ast2(): AST2 =
struct

type pos = int

fun spaces n =
    case n
     of 0 => ""
      | n => String.concat [" ", spaces (n - 1)]  

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
    
and printExp(sp, x) =
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
        String.concat [printExp(sp, e1), ".", id, 
                       "(", printExps(sp, es), ")"]
      | SimpleCall (id, retty, es, pos) => 
	String.concat [id, "(", printExps(sp, es), ")"]
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
	if pointer = "" then
	    String.concat [spaces sp, id, " = ", Exp.printExp(sp, e), ";\n"]
	else
	    String.concat [spaces sp, pointer, ".", id, " = ", Exp.printExp(sp, e), ";\n"]
      | AssignArray {id, left, right, pointer, pos} => 
	String.concat [spaces sp, id, "[", Exp.printExp(sp, left), "]", " = ", Exp.printExp(sp, right), ";\n"]
      | Return (e, pos) => String.concat [spaces sp, "return (", Exp.printExp(sp, e), ");\n"]
      | Print (e, pos) =>
        let 
	    val new_e = Exp.printExp (sp, e)
	in
	    String.concat [spaces sp, "printf (\"%d\\n\", ", new_e, ");\n"]
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
            val s2 = String.concat [spaces 8, printVars locals, "\n"]
            val s3 = Stm.printStms (8, stms)
        in  String.concat [s1, s2, s3, spaces 4, "}\n"]
        end
	
end

structure Class =
struct
datatype t
  = T of {name : string,
          cvars : {ty : Tipe.t, id : string, prefix : string} list,
          methods : Method.t list,
	  methodsTable : { className: string, 
			   methodSig: {id : string, prefix : string, rettype : Tipe.t} 
			 } list,
          pos : pos}
	 
fun compare (T {name = n1, ...}, 
             T {name = n2, ...}) = String.compare (n1, n2) 
    
fun equals (T {name = n1, ...}, 
            T {name = n2, ...}) = n1 = n2    
    
fun equalsTyVarPrefix ({ty = ty1, id = id1, prefix = p1}, 
                       {ty = ty2, id = id2, prefix = p2})
    = id1 = id2 andalso Tipe.equals (ty1, ty2)

fun printClassVar {id, ty, prefix} =
    if prefix = "" then 
	String.concat [spaces 4, Tipe.toString ty, " ", id, ";\n"]
    else 
	String.concat [spaces 4, Tipe.toString ty, " ", prefix, "_", id, ";\n"]
	
fun printClassVars vars = String.concat (List.map printClassVar vars)

fun printMethodsTable [] = ""
  | printMethodsTable ({className = c, methodSig = methodsig: {id : string, prefix : string, rettype : Tipe.t}}::ms) = 
    let 
	val {id = methodName:string, ...} = methodsig
	val mstr = String.concat [methodName, "()\n"]
	val restmstr = printMethodsTable(ms)
    in
	String.concat[mstr,restmstr]
    end
    
fun printClass (f: t) =
    case f
     of T{name=name1, cvars=cvars1, methods=methods1, methodsTable=methodsTable1, pos=pos1} =>
        let 
	    val s1 = String.concat ["class ", name1, "{", "\n"]
            val s2 = printClassVars cvars1
            val s3 = List.map Method.printMethod methods1
	    val s4 = "Method Table:\n"
	    val s5 = printMethodsTable(methodsTable1)
            val s6 = String.concat s3
        in  String.concat [s1, s4, s5, s2, s6, "}\n"]
        end
	
val bogus = T {name = "",
               cvars = [],
	       methods = [],
	       methodsTable = [],
	       pos = ~1}	
	    
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
        let val s1 = String.concat ["class ", name, "{", "\n", "\tpublic static void main(String[] ", arg, "){\n"]
            val locals_str = String.concat [spaces 8, Method.printVars locals, "\n"]
	    val s2 = List.map (fn x => Stm.printStm (8, x)) stms
            val s3 = String.concat s2
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
	    
fun findClass(className: string, []): Class.t ref = ref (Class.bogus)
  | findClass(className: string, (c::classes): Class.t list) = 
    let val Class.T{name = name1,...} = c
    in
	if name1 = className then
	    ref c
	else
	    findClass(className,classes)
    end
    
fun findMethod(methodName:string, [])= ref(Method.bogus)
  | findMethod(methodName:string, (m::ml): Method.t list)=
    let val Method.T{name = name1,...} = m
    in
	if name1 = methodName then
	    ref m
	else
	    findMethod(methodName,ml)
    end
    
fun getFunRetTipe(className: string, funName: string, f: t): Tipe.t =
    case f
     of T {mainClass, classes, pos} =>
	let val class_ref = findClass(className, classes)
	    val Class.T{methods = methods1, ...} = (!class_ref)
	    val method_ref = findMethod(funName, methods1)
	    val Method.T{rettype = rettype1,...} = (!method_ref)
	in
	    rettype1
	end
	
fun printProg f = 
    case f
     of T {mainClass, classes, pos} =>
        let val strMainClass = String.concat [MainClass.printMainClass mainClass, "\n\n\n"]
            val strClasses = String.concat (List.map (fn c => (Class.printClass c)^"\n\n\n") classes)
        in  String.concat [strMainClass, strClasses, "\n"]
        end
	
end

val printAST2 = Program.printProg

end
