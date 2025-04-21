open Types

type normal = |
type view_change = |
type recovering = |

type _ state =
  | Normal : normal state
  | ViewChange : view_change state
  | Recovering : recovering state

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) : sig
  type 's replica = {
    id : ReplicaId.t;
    config : ReplicaId.t list;
    mutable state : 's state;
    mutable view_number : ViewNumber.t;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation OpMap.t;
    mutable client_map : client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    mutable start_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable do_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable recovery_nonce : int option;
    mutable recovery_responses : ReplicaSet.t ViewMap.t;
    network : Op.operation N.t;
  }

  val init_replica : int -> int list -> normal replica
  val handle_message : 's. 's replica -> Op.operation Message.message -> unit
end
