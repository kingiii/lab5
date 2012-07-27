signature FILE =
sig
  type t
   
  val appendName : t * string -> t
  val bogus : unit -> t
  val compare : t * t -> order
  val creat : string -> t
  val equals : t * t -> bool
  val exists : t -> bool
  val println : t -> unit
  val read : t -> string
  val nameToString : t -> string
  val write : t * string -> unit
end
