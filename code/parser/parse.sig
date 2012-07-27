signature PARSE_STRUCTS =
sig
  structure Ast : AST
end

signature PARSE =
sig 
	include PARSE_STRUCTS
	val parse : string -> Ast.Program.t
end
