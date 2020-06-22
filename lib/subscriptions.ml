open Containers
open Fun
module SubsMap = Map.Make (String)

type t = {mutable state: (Messages.msg_t option -> unit) SubsMap.t}

type sid = string

let init () = {state= SubsMap.empty}

let shutdown subs = SubsMap.iter (fun _ push -> push None) subs.state

let uuid_gen =
  Random.self_init () ;
  Random.get_state () |> Uuidm.v4_gen

let next_uuid () =
  let uuid = uuid_gen () in
  Uuidm.to_string uuid |> String.filter (Char.equal '-' %> not)

let sub subscriptions =
  let state = subscriptions.state in
  let sid = next_uuid () in
  let stream, push = Lwt_stream.create () in
  subscriptions.state <- SubsMap.add sid push state ;
  (sid, stream)

let unsub sid subscriptions =
  let state = subscriptions.state in
  subscriptions.state <- SubsMap.remove sid state

let pub sid msg subscriptions =
  SubsMap.find_opt sid subscriptions.state
  |> Option.iter (fun push -> push @@ Some msg)
