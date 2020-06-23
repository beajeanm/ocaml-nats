type t

val pub : msg:string -> subject:string -> ?reply_to:string -> t -> unit Lwt.t

val sub :
     subject:string
  -> ?sid:string option
  -> ?queue:string
  -> t
  -> Messages.msg_t Lwt_stream.t Lwt.t

val req : msg:string -> subject:string -> t -> Messages.msg_t Lwt.t

val start : host:string -> port:int -> t Lwt.t

val close : t -> unit Lwt.t
