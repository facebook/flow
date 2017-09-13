module type S = sig
  val cat : string -> string
  val write_file : file:string -> contents:string -> unit
end
