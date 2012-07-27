signature GENC_STRUCTS =
sig
    structure Ast2 : AST2
    structure CAst : CAST
end

signature GENC = 
sig
    include GENC_STRUCTS
    
structure generator :
	  sig
	      exception AddYourCodeHere
	      val genPos: Ast2.pos          -> CAst.pos
	      val genBinop: Ast2.Binop.t    -> CAst.Binop.t
	      val genTipe: Ast2.Tipe.t      -> CAst.Tipe.t
	      val genExp: Ast2.Exp.t        -> CAst.Exp.t * CAst.Stm.t list
	      val genStm: Ast2.Stm.t        -> CAst.Stm.t list
	      val genMethod: Ast2.Method.t  -> unit
	      val genMainClass: Ast2.MainClass.t -> CAst.Main.t
	      val genClass: Ast2.Class.t    -> unit
	      val genProg: Ast2.Program.t   -> CAst.Program.t
	  end
	  
val gen : Ast2.Program.t -> CAst.Program.t
end
