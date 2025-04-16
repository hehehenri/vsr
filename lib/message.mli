open Types

type 'op request_message = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  op : 'op;
}

type 'op prepare_message = {
  view_number : ViewNumber.t;
  op : 'op;
  op_number : OpNumber.t;
  commit_number : OpNumber.t;
}

type prepare_ok_message = {
  view_number : ViewNumber.t;
  op_number : OpNumber.t;
  replica_id : ReplicaId.t;
}

type commit_message = {
  view_number : ViewNumber.t;
  commit_number : OpNumber.t;
}

type 'op message =
  | Request of 'op request_message
  | Prepare of 'op prepare_message
  | PrepareOk of prepare_ok_message
  | Commit of commit_message
