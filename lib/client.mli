type t

val pub : msg:string -> subject:string -> t -> unit Lwt.t

val sub : subject:string -> ?queue:string -> t -> Messages.msg_t Lwt_stream.t Lwt.t

val start : host:string -> port:int ->  t Lwt.t
