structure Main: MAIN =
struct

structure Ast = Ast()
structure Ast2 = Ast2()
structure CAst = CAst()
(*type 'a t = Ast.Program.t*)
structure E = ErrorMsg
              
structure Parse = Parse (structure Ast = Ast)
structure genAST2 = genAST2 (structure Ast = Ast; structure Ast2 = Ast2)
structure genC = genC (structure Ast2 = Ast2; structure CAst = CAst)
		    
val strParsing = "// after parsing\n\n"
exception Failed
          
fun main filename =
    let val _ = (E.reset()
               ; E.fileName := filename)
        fun say s = 
            if true 
            then print s 
            else ()
        val _ = say (String.concat ["\ncompile: ", filename, " starting\n\n"])
        val _ = say "lexing and parsing starting\n"
	val ast = Parse.parse filename
        val _ = if (!E.anyErrors) then raise E.Error else ()
        val _ = say "lexing and parsing finished\n\n"
                
        val _ = say "output ast starting\n"
	val filename_no_postfix = String.substring(filename,0,(String.size(filename)-5))
        val _ = File.write (File.creat (String.concat [filename_no_postfix, "_ast.java"]),
                            String.concat [Ast.printAST ast]) 
        val _ = if (!E.anyErrors) then raise Failed else ()
        val _ = say "output ast finished\n\n"   
        val _ = say "sort starting\n"   
	val sortedAst = Ast.sortAST(ast)
        val _ = File.write (File.creat (String.concat [filename_no_postfix, "_ast_sorted.java"]),
                            String.concat [Ast.printAST sortedAst])
        val _ = say "sort finished\n\n"   
        val _ = say "genAST2 starting\n";   
	    val ast2 = genAST2.gen(sortedAst);
	    val _ = say "genAST2 finished\n\n";   
	    val _ = File.write (File.creat (String.concat [filename_no_postfix, ".java2"]),
			                    String.concat [Ast2.printAST2 ast2])
        val _ = say "genC starting\n"   
	val C = genC.gen(ast2)
        val _ = File.write (File.creat (String.concat [filename_no_postfix, ".c"]),
                            String.concat [CAst.printASTC C])
        val _ = say "genC finished\n\n" 
	val _ = say (String.concat ["compile: ",  filename, " finished\n"])
    in ()
    end

fun mainWrap (name, args) =
    let fun doit l =
            case l
             of [] => Error.bug ("arg error")
              | [x] => print ("Usage: jc <file>\n")
              | _::x::xs => main x
        val _ = doit args
    in  OS.Process.success
    end

val _ = SMLofNJ.exportFn ("jc-compile", mainWrap)

end
