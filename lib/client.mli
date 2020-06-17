type t

type message_processor = (Messages.t, string) result -> unit Lwt.t

val pub : msg:string -> subject:string -> t -> unit Lwt.t

val sub : subject:string -> ?queue:string -> sid:string -> t -> unit Lwt.t

val start : host:string -> port:int -> message_processor -> t Lwt.t
