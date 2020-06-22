type t

type sid = string

val init : unit -> t

val shutdown : t -> unit

val sub : t -> sid * Messages.msg_t Lwt_stream.t

val unsub : sid -> t -> unit

val pub : sid -> Messages.msg_t -> t -> unit
