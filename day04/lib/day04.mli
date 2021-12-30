module Board : sig
  type t

  val make : int list list -> t

  val mark : int -> t -> unit

  val has_won : t -> bool

  val score : int -> t -> int option
end

module Part1 : sig
  val solve : int list -> Board.t list -> int
end

module Part2 : sig
  val solve : int list -> Board.t list -> int
end
