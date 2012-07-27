structure Error :> ERROR = 
struct

fun bug msg = raise (Fail (String.concat ["compiler bug: ",
                                          msg, "\n"]))
                    
fun todo msg = raise (Fail (String.concat ["unimplemented: ", msg, "\n"]))

end
