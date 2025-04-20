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

type 'op message =
  | Request of 'op request
  | Prepare of 'op prepare
  | PrepareOk of prepare_ok
  | Commit of commit
