signature AST2_STRUCTS =
sig
end

signature AST2 = 
sig
    include AST2_STRUCTS
    
type pos = int
	   
structure Binop:
	  sig
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
		  
	      val toString: t -> string
	  end

structure Tipe:
	  sig
	      datatype t
		= Int
		| Bool
		| ArrayInt
		| Name of string
			  
	      val equals: t * t -> bool
	      val toString: t -> string
	  end
	  
structure Exp :
	  sig
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
			      
	      val printExp: (int*t) -> string
	  end  

structure Stm :
	  sig
	      datatype t
		= AssignId of {id: string, e: Exp.t, pointer: string, pos: pos}
		| AssignArray of {id: string, left: Exp.t, right: Exp.t, pointer: string, pos: pos}
		| If of Exp.t * t list * t list * pos
		| While of Exp.t * t list * pos
		| Return of Exp.t * pos
		| Print of Exp.t * pos
			   
	      val printStm : (int*t) -> string
	  end
	  
structure Method :
	  sig
	      datatype t
		= T of {name : string,
			prefix : string,
			rettype : Tipe.t,
			args : {id : string, ty : Tipe.t} list,
			locals : {id : string, ty : Tipe.t} list,
			stms : Stm.t list,
			pos : pos}
		       
	      val bogus: t
	      val compare: t * t -> order
	      val equals: t * t -> bool
	  end
	  
structure Class :
	  sig
	      datatype t
		= T of {name : string,
			cvars : {ty : Tipe.t, id : string, prefix : string} list, methods : Method.t list, 	
			methodsTable : { className: string, 
					 methodSig: {id : string, prefix : string, rettype : Tipe.t} 
				       } list,
			pos : pos}
		       
	      val compare: t * t -> order
	      val equals: t * t -> bool
	      val equalsTyVarPrefix : {ty : Tipe.t, id : string, prefix : string} 
				      * {ty : Tipe.t, id : string, prefix : string} -> bool
	      val bogus: t
			 
	  end
	  
structure MainClass :
	  sig
	      datatype t
		= T of {name: string,
			arg: string,
			locals: {id: string, ty: Tipe.t} list,
			stms: Stm.t list,
			pos: pos}
		       
	      val bogus: t
	  end
	  
structure Program :
	  sig
	      datatype t
		= T of {mainClass : MainClass.t,
			classes : Class.t list,
			pos : pos}
		       
	      val bogus: t
	      val getFunRetTipe: string * string * t -> Tipe.t
	  end
	  
val printAST2 : Program.t -> string 
			     
end
