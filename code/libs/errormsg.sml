structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val file = ref (File.bogus ())
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun reset() = (anyErrors:=false;
		 file := File.bogus ();
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)

  exception Error

  fun error (pos,msg:string) =
      let fun look(a::rest,n) =
	      if a<pos 
	      then List.app print [": ",
			           Int.toString n,
			           ".",
			           Int.toString (pos-a)]
	      else look(rest,n-1)
	    | look _ = print "0.0"
      in  anyErrors := true;
	  print (File.nameToString (!file));
	  print " ";
	  look(!linePos,!lineNum);
	  print " : ";
	  print msg;
	  print "\n"
      end

  fun warning pos (msg:string) =
      let fun look(a::rest,n) =
	      if a<pos 
	      then List.app print [":",
			           Int.toString n,
			           ".",
			           Int.toString (pos-a)]
	      else look(rest,n-1)
	    | look _ = print "0.0"
      in  print (File.nameToString (!file));
          print " ";
	  look(!linePos,!lineNum);
	  print ":";
	  print msg;
	  print "\n"
      end

  fun impossible msg =
      (List.app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
  
end  (* structure ErrorMsg *)


