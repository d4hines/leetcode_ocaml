module type ORD = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Make (Ord : ORD) : sig
  type t = Empty | Node of { v : Ord.t; left : t; right : t }
  [@@deriving show { with_path = false }]

  val empty : t
  val singleton : Ord.t -> t
  val union : t -> t -> t
  val insert : Ord.t -> t -> t
  val pop : t -> (Ord.t * t) option
end
