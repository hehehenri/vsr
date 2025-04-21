open Types

type config = ReplicaId.t list

module type S = sig
  type ('op, 'result) t

  type destination =
    | Single of ReplicaId.t
    | Broadcast
    | Client of ClientId.t

  val create : config -> ('op, 'result) t

  val send : ('op, 'result) t -> destination -> ('op, 'result) Message.message -> unit

  val broadcast : ('op, 'result) t -> ('op, 'result) Message.message -> unit

  val drain_messages : ('op, 'result) t -> ReplicaId.t -> ('op, 'result) Message.message list

  val has_messages : ('op, 'result) t -> ReplicaId.t -> bool

  val get_config : ('op, 'result) t -> ReplicaId.t list
end

module InMemory = struct
  module ReplicaMap = Map.Make (struct
    type t = ReplicaId.t
    let compare = ReplicaId.compare
  end)

  module ClientMap = Map.Make (struct
    type t = ClientId.t
    let compare = ClientId.compare
  end)

  type ('op, 'result) message_queue = ('op, 'result) Message.message Queue.t

  type ('op, 'result) t = {
    config : ReplicaId.t list;
    mutable replica_queues : ('op, 'result) message_queue ReplicaMap.t;
    mutable client_queues : ('op, 'result) message_queue ClientMap.t;
  }

  type destination = Single of ReplicaId.t | Broadcast | Client of ClientId.t

  let create config =
    let replica_queues =
      List.fold_left
        (fun acc id ->
          let q = Queue.create () in
          ReplicaMap.add id q acc)
        ReplicaMap.empty config
    in
    { config; replica_queues; client_queues = ClientMap.empty }

  let send t dest msg =
    match dest with
    | Single id ->
        let queue = ReplicaMap.find id t.replica_queues in
        Queue.add msg queue
    | Client client_id ->
        let queue = 
          match ClientMap.find_opt client_id t.client_queues with
          | Some q -> q
          | None ->
              let q = Queue.create () in
              t.client_queues <- ClientMap.add client_id q t.client_queues;
              q
        in
        Queue.add msg queue
    | Broadcast -> ReplicaMap.iter (fun _ queue -> Queue.add msg queue) t.replica_queues

  let broadcast t msg = send t Broadcast msg

  let drain_messages t id =
    let queue = ReplicaMap.find id t.replica_queues in
    let messages = ref [] in
    while not (Queue.is_empty queue) do
      messages := Queue.take queue :: !messages
    done;
    List.rev !messages

  let has_messages t id =
    let queue = ReplicaMap.find id t.replica_queues in
    not (Queue.is_empty queue)

  let get_config t = t.config
end

open Types

module ReplicaMap = Map.Make (struct
  type t = ReplicaId.t

  let compare = ReplicaId.compare
end)

type ('op, 'result) t = {
  config : config;
  messages : ('op, 'result) Message.message list ReplicaMap.t;
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
