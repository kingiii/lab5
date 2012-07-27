functor Parse (S: PARSE_STRUCTS): PARSE =
struct

open S
     
val error = ErrorMsg.error
	    
structure JavaLrVals = JavaLrValsFun(
  structure Token = LrParser.Token
  structure Ast = Ast)
structure Lex = JavaLexFun(structure Tokens = JavaLrVals.Tokens)
structure CP = Join(
  structure ParserData = JavaLrVals.ParserData
  structure Lex=Lex
  structure LrParser = LrParser)

fun parse filename =
    let val file = (TextIO.openIn filename) 
	    handle Io => (raise Fail "compiler bug\n")
        fun get _ = TextIO.input file
        fun parseerror (s, p1, p2) = ErrorMsg.error(p1,s)
        val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	val _ = print"lexer finish!\n"
        val result = #1(CP.parse(30, lexer, parseerror, ()))
    in  result
    end handle LrParser.ParseError => 
               (print(String.concat ["Parsing Error on ",
                                     !ErrorMsg.fileName,
                                     "Compiling terminated!\n"])
              ; ErrorMsg.anyErrors := true;
		Ast.Program.bogus)
               
end
