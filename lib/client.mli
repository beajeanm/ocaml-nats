type t

val pub : msg:string -> subject:string -> t -> unit Lwt.t

val sub : subject:string -> ?queue:string -> sid:string -> t -> unit Lwt.t

val start : host:string -> port:int -> (Messages.t -> unit Lwt.t) -> (t * unit Lwt.t) Lwt.t
