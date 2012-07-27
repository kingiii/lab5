structure File :> FILE =
struct
  datatype t
    = T of string
  
  fun appendName (T s, str) = T (String.concat [s, str])
  
  fun bogus () = T ""

  fun compare (T k1, T k2) = String.compare (k1, k2)

  fun creat s = T s
      
  fun equals (T k1, T k2) = (k1 = k2)
  
  fun exists (T s) =
      let val flag = ref true
          val _ = (TextIO.closeIn (TextIO.openIn s)) handle Io => (flag := false)
      in  !flag
      end
    
  fun read (T s) =
      let val inStream = TextIO.openIn s
                         handle Io => raise Fail (String.concat ["open input file failed : ", s])
          val (str) = TextIO.input inStream
      in  TextIO.closeIn inStream
        ; str
      end
    
  fun println f = print (String.concat [read f, "\n"])
  
  fun nameToString (T s) = s
  
  fun write (T s, str) =
      let val outStream = TextIO.openOut s
                          handle Io => raise Fail (String.concat ["open output file failed : ", s])
          val _ = TextIO.output (outStream, str)
      in  TextIO.closeOut outStream
      end
end
