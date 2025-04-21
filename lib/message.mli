open Types

type ('op, 'result) response = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  result : 'result;
}

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
  log : 'op request OpMap.t;
  last_view_number : ViewNumber.t;
  op_number : OpNumber.t;
  commit_number : OpNumber.t;
}

type recovery = {
  replica_id: ReplicaId.t;
  nonce: int;
}

type 'op recovery_response_data = {
  log: 'op request OpMap.t;
  op_number: OpNumber.t;
  commit_number: OpNumber.t;
}

type 'op recovery_response = {
  replica_id: ReplicaId.t;
  view_number: ViewNumber.t;
  nonce: int;
  response_type: [ 
    | `Primary of 'op recovery_response_data
    | `NonPrimary
  ];
}

type get_state = {
  view_number : ViewNumber.t;
  op_number : OpNumber.t;
  replica_id : ReplicaId.t;
}

type 'op new_state = {
  view_number : ViewNumber.t;
  log : 'op request OpMap.t;
  op_number : OpNumber.t;
  commit_number : OpNumber.t;
}

type 'op replica_message =
  | Prepare of 'op prepare
  | PrepareOk of prepare_ok
  | Commit of commit
  | StartViewChange of start_view_change
  | DoViewChange of 'op do_view_change
  | Recovery of recovery
  | RecoveryResponse of 'op recovery_response
  | GetState of get_state
  | NewState of 'op new_state

type ('op, 'result) client_message =
  | Request of 'op request
  | Response of ('op, 'result) response

type ('op, 'result) message =
  | ReplicaMessage of 'op replica_message
  | ClientMessage of ('op, 'result) client_message
