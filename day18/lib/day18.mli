module Tree : sig
  type t = private {left : elem; right : elem}

  and elem = private Atom of int | Tree of t

  val atom : int -> elem

  val tree : t -> elem

  val make : elem -> elem -> t

  val reduce : t -> t

  val magnitude : t -> int

  val parse : string -> t

  val pp : Format.formatter -> t -> unit
end

module Part1 : sig
  val solve : Tree.t list -> int
end

module Part2 : sig
  val solve : Tree.t list -> int
end
