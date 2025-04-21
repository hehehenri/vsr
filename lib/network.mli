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

module InMemory : S

module ReplicaMap : Map.S with type key = ReplicaId.t

type ('op, 'result) t = {
  config : config;
  messages : ('op, 'result) Message.message list ReplicaMap.t;
}

val create : config -> ('op, 'result) t

val send_message : ('op, 'result) t -> to_:ReplicaId.t -> ('op, 'result) Message.message -> ('op, 'result) t

val broadcast : ('op, 'result) t -> from:ReplicaId.t -> ('op, 'result) Message.message -> ('op, 'result) t

val get_messages : ('op, 'result) t -> ReplicaId.t -> ('op, 'result) Message.message list

val drain_messages : ('op, 'result) t -> ReplicaId.t -> ('op, 'result) t

val has_messages : ('op, 'result) t -> ReplicaId.t -> bool

val replicas_with_messages : ('op, 'result) t -> ReplicaId.t list

val message_count : ('op, 'result) t -> int