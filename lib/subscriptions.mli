type t

type sid = string

val init : unit -> t

val shutdown : t -> unit

val next_sid : t -> sid

val sub : sid -> t -> Messages.msg_t Lwt_stream.t

val unsub : sid -> t -> unit

val pub : sid -> Messages.msg_t -> t -> unit
