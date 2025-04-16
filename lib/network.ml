open Types

type config = ReplicaId.t list

module type S = sig
  type 'op t
  type destination = Single of ReplicaId.t | Broadcast

  val create : config -> 'op t
  val send : 'op t -> destination -> 'op Message.message -> unit
  val broadcast : 'op t -> 'op Message.message -> unit
  val drain_messages : 'op t -> ReplicaId.t -> 'op Message.message list
  val has_messages : 'op t -> ReplicaId.t -> bool
  val get_config : 'op t -> ReplicaId.t list
end

module InMemory = struct
  module ReplicaMap = Map.Make (struct
    type t = ReplicaId.t

    let compare = ReplicaId.compare
  end)

  type 'op message_queue = 'op Message.message Queue.t

  type 'op t = {
    config : ReplicaId.t list;
    mutable queues : 'op message_queue ReplicaMap.t;
  }

  type destination = Single of ReplicaId.t | Broadcast

  let create config =
    let queues =
      List.fold_left
        (fun acc id ->
          let q = Queue.create () in
          ReplicaMap.add id q acc)
        ReplicaMap.empty config
    in
    { config; queues }

  let send t dest msg =
    match dest with
    | Single id ->
        let queue = ReplicaMap.find id t.queues in
        Queue.add msg queue
    | Broadcast -> ReplicaMap.iter (fun _ queue -> Queue.add msg queue) t.queues

  let broadcast t msg = send t Broadcast msg

  let drain_messages t id =
    let queue = ReplicaMap.find id t.queues in
    let messages = ref [] in
    while not (Queue.is_empty queue) do
      messages := Queue.take queue :: !messages
    done;
    List.rev !messages

  let has_messages t id =
    let queue = ReplicaMap.find id t.queues in
    not (Queue.is_empty queue)

  let get_config t = t.config
end

open Types

module ReplicaMap = Map.Make (struct
  type t = ReplicaId.t

  let compare = ReplicaId.compare
end)

type 'op t = {
  config : config;
  messages : 'op Message.message list ReplicaMap.t;
}

let create config = { config; messages = ReplicaMap.empty }

let send_message network ~to_ msg =
  let current_messages =
    ReplicaMap.find_opt to_ network.messages |> Option.value ~default:[]
  in
  {
    network with
    messages = ReplicaMap.add to_ (msg :: current_messages) network.messages;
  }

let broadcast network ~from msg =
  List.fold_left
    (fun net to_ ->
      if to_ = from then net
      else send_message net ~to_ msg)
    network network.config

let get_messages network replica_id =
  ReplicaMap.find_opt replica_id network.messages |> Option.value ~default:[]

let drain_messages network replica_id =
  { network with messages = ReplicaMap.remove replica_id network.messages }

let has_messages network replica_id =
  match ReplicaMap.find_opt replica_id network.messages with
  | None -> false
  | Some msgs -> msgs <> []

let replicas_with_messages network =
  ReplicaMap.fold
    (fun replica_id msgs acc -> if msgs <> [] then replica_id :: acc else acc)
    network.messages []

let message_count network =
  ReplicaMap.fold
    (fun _ msgs count -> count + List.length msgs)
    network.messages 0
