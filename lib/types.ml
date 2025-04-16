module ClientId = struct
  type t = ClientId of int

  let to_int (ClientId t) = t
  let of_int i = ClientId i
  let compare (ClientId a) (ClientId b) = compare a b
end

module ReplicaId = struct
  type t = ReplicaId of int

  let to_int (ReplicaId t) = t
  let of_int i = ReplicaId i
  let compare (ReplicaId a) (ReplicaId b) = compare a b
end

module OpNumber = struct
  type t = OpNumber of int

  let init () = OpNumber 0
  let compare (OpNumber a) (OpNumber b) = compare a b
  let succ (OpNumber t) = OpNumber (succ t)
end

module RequestNumber = struct
  type t = RequestNumber of int

  let init () = RequestNumber 0
  let compare (RequestNumber a) (RequestNumber b) = compare a b
  let succ (RequestNumber t) = RequestNumber (succ t)
end

module ViewNumber = struct
  type t = ViewNumber of int

  let init () = ViewNumber 0
  let compare (ViewNumber a) (ViewNumber b) = compare a b
  let succ (ViewNumber t) = ViewNumber (succ t)
  let to_int (ViewNumber t) = t
end