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