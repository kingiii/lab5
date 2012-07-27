signature MAIN =
sig
    val main: string -> unit
    val mainWrap: string * string list -> OS.Process.status
end
