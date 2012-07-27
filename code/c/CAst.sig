signature CAST_STRUCTS =
sig
end

signature CAST = 
sig
    include CAST_STRUCTS
    
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
			  
	    val equals: t * t   -> bool
	    val toString: t     -> string
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
		| Malloc of t * pos
		| Sizeof of t * pos
		| ParenExp of t * pos
			      
	    val printExp: int*t -> string
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
			   
	    val printStm : int*t -> string
	  end
	  
structure Method :
	  sig
	    datatype t
		= T of {name: string,
			    prefix: string,
			    rettype: Tipe.t,
			    args: {id: string, ty: Tipe.t} list,
			    locals: {id: string, ty: Tipe.t} list,
			    stms: Stm.t list,
			    pos: pos}
		       
	    val bogus: t
	  end
	  
structure Main :
	  sig
	    datatype t
		= T of {locals: {id: string, ty: Tipe.t} list,
			    stms: Stm.t list,
			    pos: pos}
	  end
	  
structure Struct_Vtable :
	  sig
	    datatype t
		= T of {name: string,
			    fields: {id: string, prefix: string, rettype: Tipe.t} list,
			    pos: pos}
	  end	  

structure Struct :
	  sig
	    datatype t
		= T of {name : string,
			    vptr : Struct_Vtable.t ref,
			    fields: {ty: Tipe.t, id: string, prefix: string} list,
			    pos: pos}
	  end
	  
structure Program :
	  sig
	    datatype t
		= T of {globals: {ty: Tipe.t, id: string} list,
			    structs: Struct.t list,
			    structs_vtbls: Struct_Vtable.t list,
			    methods: Method.t list,
			    main: Main.t,
			    initVtbl: Method.t,
			    pos: pos}
	    val bogus: t
	  end
	  
val printASTC : Program.t -> string 
			     
end
