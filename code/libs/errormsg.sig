signature ERRORMSG_STRUCTS =
sig
end

signature ERRORMSG =
sig
  include ERRORMSG_STRUCTS
  
  val anyErrors : bool ref
  val file : File.t ref
  val fileName : string ref
  val lineNum : int ref
  val linePos : int list ref
  val sourceStream : TextIO.instream ref
  val error : int * string -> unit
  val warning : int -> string -> unit
  exception Error
  val impossible : string -> 'a   (* raises Error *)
  val reset : unit -> unit
end

