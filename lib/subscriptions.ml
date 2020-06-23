open Containers
open Fun
module SubsMap = Map.Make (String)

type t =
  { mutable state: (Messages.msg_t option -> unit) SubsMap.t
  ; id_generator: unit -> Uuidm.t }

type sid = string

let uuid_gen () =
  Random.self_init () ;
  Random.get_state () |> Uuidm.v4_gen

let init () = {state= SubsMap.empty; id_generator= uuid_gen ()}

let shutdown subs = SubsMap.iter (fun _ push -> push None) subs.state

let next_sid subs =
  let uuid = subs.id_generator () in
  Uuidm.to_string uuid |> String.filter (Char.equal '-' %> not)

let sub sid subs =
  let state = subs.state in
  let stream, push = Lwt_stream.create () in
  subs.state <- SubsMap.add sid push state ;
  stream

let unsub sid subs =
  let state = subs.state in
  subs.state <- SubsMap.remove sid state

let pub sid msg subs =
  SubsMap.find_opt sid subs.state |> Option.iter (fun push -> push @@ Some msg)
