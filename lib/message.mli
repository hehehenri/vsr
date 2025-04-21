open Types

type 'op request = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  op : 'op;
}

type 'op prepare = {
  view_number : ViewNumber.t;
  op_number : OpNumber.t;
  commit_number : OpNumber.t;
  request : 'op request;
}

type prepare_ok = {
  view_number : ViewNumber.t;
  op_number : OpNumber.t;
  replica_id : ReplicaId.t;
}

type commit = { view_number : ViewNumber.t; commit_number : OpNumber.t }

type start_view_change = {
  view_number : ViewNumber.t;
  replica_id : ReplicaId.t;
}

type 'op do_view_change = {
  view_number : ViewNumber.t;
  log : 'op OpMap.t;
  last_view_number : ViewNumber.t;
  op_number : OpNumber.t;
  commit_number : OpNumber.t;
}

type recovery = {
  replica_id: ReplicaId.t;
  nonce: int;
}

type 'op recovery_response = {
  replica_id: ReplicaId.t;
  view_number: ViewNumber.t;
  nonce: int;
  log: 'op OpMap.t Option.t;
  op_number: OpNumber.t Option.t;
  commit_number: OpNumber.t Option.t;
}

type 'op message =
  | Request of 'op request
  | Prepare of 'op prepare
  | PrepareOk of prepare_ok
  | Commit of commit
  | StartViewChange of start_view_change
  | DoViewChange of 'op do_view_change
  | Recovery of recovery
  | RecoveryResponse of 'op recovery_response
