module ClientId : sig
  type t = ClientId of int

  val to_int : t -> int
  val of_int : int -> t
  val compare : t -> t -> int
end

module ReplicaId : sig
  type t = ReplicaId of int

  val to_int : t -> int
  val of_int : int -> t
  val compare : t -> t -> int
end

module OpNumber : sig
  type t = OpNumber of int

  val init : unit -> t
  val succ : t -> t
  val compare : t -> t -> int
end

module RequestNumber : sig
  type t = RequestNumber of int

  val init : unit -> t
  val succ : t -> t
  val compare : t -> t -> int
end

module ViewNumber : sig
  type t = ViewNumber of int

  val init : unit -> t
  val succ : t -> t
  val compare : t -> t -> int
  val to_int : t -> int
end

module ViewMap : Map.S with type key = ViewNumber.t
module OpMap : Map.S with type key = OpNumber.t
module ClientMap : Map.S with type key = ClientId.t
module ReplicaSet : Set.S with type elt = ReplicaId.t

type client_entry = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  response : string option;
}

type client_map = client_entry ClientMap.t

