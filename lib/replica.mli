open Types

type normal = |
type view_change = |
type recovering = |

type _ state =
  | Normal : normal state
  | ViewChange : view_change state
  | Recovering : recovering state

type config = ReplicaId.t list

module OpMap : Map.S with type key = OpNumber.t
module ClientIdMap : Map.S with type key = ClientId.t
module ReplicaSet : Set.S with type elt = ReplicaId.t

type client_entry = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  response : string option;
}

type client_map = client_entry ClientIdMap.t

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) : sig
  type 's replica = {
    id : ReplicaId.t;
    state : 's state;
    view : ViewNumber.t;
    config : config;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation OpMap.t;
    mutable client_map : client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    network : Op.operation N.t;
  }

  val init_replica : int -> int list -> normal replica
  val get_primary_id : 's replica -> ReplicaId.t
  val is_primary : 's replica -> bool
  val handle_message : 's. 's replica -> Op.operation Message.message -> unit
end
