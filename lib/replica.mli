open Types

type state =
  | Normal
  | ViewChange
  | Recovering

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) : sig
  type replica = {
    id : ReplicaId.t;
    config : ReplicaId.t list;
    mutable state : state;
    mutable view_number : ViewNumber.t;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation Message.request OpMap.t;
    mutable client_map : (Op.operation, Op.result) client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    mutable start_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable do_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable recovery_nonce : int option;
    mutable recovery_responses : ReplicaSet.t ViewMap.t;
    network : (Op.operation, Op.result) N.t;
  }

  val init_replica : int -> int list -> replica
  val handle_message : replica -> (Op.operation, Op.result) Message.message -> unit
end
