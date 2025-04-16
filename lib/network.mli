open Types

type config = ReplicaId.t list

module type S = sig
  type 'op t

  type destination =
    | Single of ReplicaId.t
    | Broadcast

  val create : config -> 'op t

  val send : 'op t -> destination -> 'op Message.message -> unit

  val broadcast : 'op t -> 'op Message.message -> unit

  val drain_messages : 'op t -> ReplicaId.t -> 'op Message.message list

  val has_messages : 'op t -> ReplicaId.t -> bool

  val get_config : 'op t -> ReplicaId.t list
end

module InMemory : S

module ReplicaMap : Map.S with type key = ReplicaId.t

type 'op t = {
  config : config;
  messages : 'op Message.message list ReplicaMap.t;
}

val create : config -> 'op t

val send_message : 'op t -> to_:ReplicaId.t -> 'op Message.message -> 'op t

val broadcast : 'op t -> from:ReplicaId.t -> 'op Message.message -> 'op t

val get_messages : 'op t -> ReplicaId.t -> 'op Message.message list

val drain_messages : 'op t -> ReplicaId.t -> 'op t

val has_messages : 'op t -> ReplicaId.t -> bool

val replicas_with_messages : 'op t -> ReplicaId.t list

val message_count : 'op t -> int