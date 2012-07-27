functor CAst(): CAST =
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
      | ArrayInt => "int *"
      | Name s => String.concat ["struct ", s]
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
 fn Plus    => " + "
  | Minus   => " - "
  | Times   => " * "
  | Div     => "/"
  | Mod     => "%"
  | And     => " && "
  | Or      => "||"
  | Gt      => " > "
  | Lt      => " < "
  | Eq      => "=="
  | Ge      => ">="
  | Le      => "<="
  | Neq     => "!="
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
  | Malloc of t * pos
  | Sizeof of t * pos
  | ParenExp of t * pos

fun printExp (sp: int, x): string =
    case x
     of Inti (i)    => Int.toString i
      | True        => "1"
      | False       => "0"
      | Var {id, pointer, pos} => if pointer = ""
                                  then id
                                  else String.concat [pointer, "->", id]
      | Binop (e1, b, e2, pos) => String.concat [printExp (sp, e1),
                                                Binop.toString b,
                                                printExp(sp,e2)]
      | NewArray (e, pos)   => printExp(sp,e)
      | NewId (id, pos)     => id
      | This pos            => "this"           
      | Not (e, pos)        => String.concat ["!(", printExp(sp,e), ")"]
      | Length (e, pos)     => String.concat [printExp(sp,e), ".length"]
      | Call (e1, ety, id, retty, es, pos)  => String.concat [printExp(sp,e1),
                                                              "->",
                                                              id,
                                                              "(",
                                                              printExps(sp,es),
                                                              ")"]
      | SimpleCall (id, retty, es, pos)     => String.concat [id,
                                                              "(",
                                                              printExps(sp, es),
                                                              ")"]
      | Array (e1, e2, pos) => String.concat [printExp(sp,e1),
                                              "[",
                                              printExp(sp,e2),
                                              "]"]   
      | Malloc (e, pos)     => String.concat ["malloc(",
                                              printExp(sp,e),
                                              ")"]  
      | Sizeof (e, pos)     => String.concat ["sizeof(",
                                              printExp(sp,e),
                                              ")"]  
      | ParenExp (e, pos)   => String.concat ["(",
                                              printExp(sp,e),
                                              ")"]
  and printExps (sp: int, []) = ""
    | printExps (sp: int, es) = 
      let
        val size: int = List.length es
	    val e::ex = es
      in
	    if size = 1
        then printExp(sp, e)
	    else printExp(sp, e) ^ ", " ^ printExps(sp,ex)
      end
end

structure Stm =
struct
datatype t
  = AssignId of {id: string, e: Exp.t, pointer: string, pos: pos}
  | AssignArray of {id: string, left: Exp.t, right : Exp.t, pointer: string, pos: pos}
  | If of Exp.t * t list * t list * pos
  | While of Exp.t * t list * pos
  | Return of Exp.t * pos
  | Print of Exp.t * pos
             
fun printStm (sp, x) =
    case x
     of If (e, ss1, ss2, pos) => 
                  String.concat [spaces sp,
                                "if ", "(",
                                Exp.printExp(sp, e) ,
                                "){\n",
				                printStms (sp + 4, ss1), spaces sp,
                                "}\n",
				                spaces sp,
                                "else{\n",
				                printStms (sp + 4, ss2), spaces sp,
                                "}\n"]
      | While (e, ss, pos) =>
            let
              val e_str = Exp.printExp (sp, e)
              val stms_str = printStms (sp + 4, ss)
            in
	          String.concat [spaces sp,
                            "while ",
                            "(",
                            e_str,
                            "){\n",
			                stms_str, spaces sp,
                            "}\n"]
            end
      | AssignId {id, e, pointer, pos} =>
	        if pointer = "" 
            then String.concat [spaces sp, id,
                                " = ",
                                Exp.printExp(sp, e),
                                ";\n"]
	        else String.concat [spaces sp, pointer,
                                "->",
                                id,
                                " = ",
                                Exp.printExp(sp, e),
                                ";\n"]
      | AssignArray {id, left, right, pointer, pos} =>
          if pointer = ""
          then String.concat [spaces sp, id,
                              "[",
                              Exp.printExp(sp, left),
                              "]", " = ",
                              Exp.printExp(sp, right),
                              ";\n"]
          else String.concat [spaces sp, pointer,
                              "->",
                              id,
                              "[",
                              Exp.printExp(sp, left),
                              "]", " = ",
                              Exp.printExp(sp, right),
                              ";\n"]

      | Return (e, pos) => String.concat [spaces sp,
                                          "return (",
                                          Exp.printExp(sp, e),
                                          ");\n"]
      | Print (e, pos) =>
            let 
	          val new_e = Exp.printExp (sp, e)
	        in
	          String.concat [spaces sp,
                            "printf (\"%d\\n\", ",
                            new_e,
                            ");\n"]
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
	 
val bogus = T {name     = "bogus",
               prefix   = "",
               rettype  = Tipe.Int,
               args     = [],
               locals   = [],
               stms     = [],
               pos      = ~1}

fun printVar(sp:int, {id, ty}) = String.concat [spaces sp, Tipe.toString ty, " ", id, ";\n"]

fun printVars (sp, vars) = String.concat (List.map (fn x => printVar(sp, x)) vars)
			   
fun printArgs args =
    case args
     of []              => ""
      |[{id, ty}]       => String.concat [Tipe.toString ty, " ", id] 
      | {id, ty}::xs    =>
        String.concat [Tipe.toString ty, " ", id, ", ", printArgs xs]    			
	
fun printMethod f =
    case f
     of T {name, prefix, rettype, args, locals, stms, pos} =>
        let
          val s3 = Stm.printStms (4, stms)
          val s2 = String.concat [printVars(4, locals), "\n"]
	      val s1 = 
            if prefix = ""
            then String.concat [Tipe.toString rettype,
                                " ", 
					            name,
                                " (",
                                printArgs args,
                                ") ", "{\n"]
            else String.concat [Tipe.toString rettype,
                                " ", 
                                prefix,
                                "_",
                                name,
                                " (",
                                printArgs args,
                                ") ", "{\n"]
        in
          String.concat [s1, s2, s3, "}\n"]
        end
end

structure Main =
struct
datatype t
  = T of {locals : {id: string, ty: Tipe.t} list,
	  stms : Stm.t list,
          pos : pos}
         
val bogus = T {locals = [], stms = [], pos = 0} 

fun printMain f =
    case f
     of T {locals, stms, pos} => 
        let
	      val main = String.concat ["void main(int argc, char* argv[]){\n"]
	      val init =  (spaces 4) ^ "initVtables ();\n"
          val localvar = Method.printVars(4, locals)
          val stm = List.map (fn x => Stm.printStm (4, x)) stms
          val stmList = String.concat stm
        in  
	      String.concat [main, localvar, init, stmList, "}\n"]
        end

end

structure Struct_Vtable =
struct
datatype t
  = T of {name : string,
          fields : {id : string, prefix : string, rettype : Tipe.t} list,
	      pos: pos}
	 
fun printStruct_Vtable f =
    case f
     of T {name, fields, pos} =>
        let
          val s1 = String.concat ["struct ", name, "{\n"]
          val s2 = List.map (fn {id, prefix, rettype} =>
                                  String.concat [spaces 4, Tipe.toString rettype,
                                                " ", "(* ", id, ")", "();\n"]) fields
          val s3 = String.concat s2
        in 
          String.concat [s1, s3, "};\n"]
        end

end

structure Struct =
struct
datatype t
  = T of {name : string,
          vptr : Struct_Vtable.t ref,
	      fields: {ty: Tipe.t, 
                   id: string, 
                   prefix: string} list, 
          pos: pos}
	 
fun printStruct f =
    case f
     of T {name, vptr, fields, pos} =>
        let 
	      val Struct_Vtable.T {name = vptrName, ... } = !vptr
	      val strVptr = String.concat [spaces 4, "struct ", vptrName, " * ", "vptr;\n"]
          val structName = String.concat ["struct ", name, "{", "\n"]
          val s2 = List.map (fn {id, ty, prefix} =>
                                  if prefix = ""
                                  then String.concat [spaces 4, Tipe.toString ty, " ", id, ";\n"]
                                  else String.concat [spaces 4, Tipe.toString ty, " ", prefix, "_", id, ";\n"]) fields
          val s3 = String.concat s2
        in
          String.concat [structName, strVptr, s3, "};\n"]
        end
end

structure Program =
struct
datatype t
  = T of {globals: {ty: Tipe.t, id: string} list,
	      structs: Struct.t list,
	      structs_vtbls: Struct_Vtable.t list,
          methods: Method.t list,
          main: Main.t,
          initVtbl: Method.t,
          pos: pos}
         
val bogus = T {structs          = [],
               globals          = [],
	           structs_vtbls    = [],
               methods          = [],
               main             = Main.bogus,
	           initVtbl         = Method.bogus,
               pos              = ~1}

fun includeCLib () =
    String.concat ["#include <stdio.h>\n","#include <stdlib.h>\n\n"]    
    
fun printProg f = 
    case f
     of T {structs, structs_vtbls, methods, main, globals, initVtbl, pos} =>
        let
          val strLib = includeCLib ()
	      val strStructsVtbl = String.concat (List.map (fn c => (Struct_Vtable.printStruct_Vtable c) ^ "\n") structs_vtbls)
          val strStructs = String.concat (List.map (fn c => (Struct.printStruct c)^"\n") structs)
          val strDecVtable = Method.printVars(0, globals)
          val strInitVtables = Method.printMethod(initVtbl)
	      val strMethods = String.concat (List.map (fn c => (Method.printMethod c)^"\n") methods)
	      val strMain = String.concat [Main.printMain main, "\n"]
        in
          String.concat [strLib, strStructsVtbl, strStructs, strDecVtable, "\n", strMethods, strInitVtables, "\n", strMain, "\n"]
        end
	
end

val printASTC = Program.printProg

end
