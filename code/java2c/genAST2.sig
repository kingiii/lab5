signature GENAST2_STRUCTS =
sig
    structure Ast : AST
    structure Ast2 : AST2
end

signature GENAST2 = 
sig
    include GENAST2_STRUCTS
    
structure generator :
	  sig
	      exception Unimplemented
	      val genPos: Ast.pos -> Ast.pos
	      val genBinop: Ast.Binop.t -> Ast2.Binop.t
	      val genTipe: Ast.Tipe.t -> Ast2.Tipe.t
	      val genExp: Ast.Exp.t -> Ast2.Exp.t
	      val genVars: {id:string, ty:Ast.Tipe.t} list -> {id:string, ty:Ast2.Tipe.t} list
	      val genClassVars: {id:string, ty:Ast.Tipe.t, prefix:string} list -> {id:string, ty:Ast2.Tipe.t, prefix:string} list
	      val genStm: Ast.Stm.t -> Ast2.Stm.t
	      val genMethod: Ast.Method.t -> Ast2.Method.t
	      val genMainClass: Ast.MainClass.t -> Ast2.MainClass.t
	      val genClasses: Ast.Class.t list -> Ast2.Class.t list
	      val genProg: Ast.Program.t -> Ast2.Program.t
	  end
	  
val gen : Ast.Program.t -> Ast2.Program.t
end
