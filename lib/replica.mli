open Types

type normal = |
type view_change = |
type recovering = |

type _ state =
  | Normal : normal state
  | ViewChange : view_change state
  | Recovering : recovering state

type config = ReplicaId.t list

module LogMap : Map.S with type key = OpNumber.t

module ClientIdMap : Map.S with type key = ClientId.t

type client_entry = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  response : string option;
}

type client_map = client_entry ClientIdMap.t

module Make (N : Network.S) : sig
  type ('s, 'op) replica = {
    id : ReplicaId.t;
    state : 's state;
    view : ViewNumber.t;
    config : config;
    mutable op_number : OpNumber.t;
    commit_number : OpNumber.t;
    mutable request_log : 'op Message.request_message LogMap.t;
    mutable client_map : client_map;
    network : 'op N.t;
  }

  val init_replica : int -> int list -> (normal, 'op) replica

  val get_primary_id : ('s, 'op) replica -> ReplicaId.t

  val is_primary : ('s, 'op) replica -> bool

  val handle_request : ('s, 'op) replica -> 'op Message.request_message -> unit

  val handle_message : ('s, 'op) replica -> 'op Message.message -> unit
end
