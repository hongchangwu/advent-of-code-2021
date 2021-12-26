module Board : sig
  type t

  val make : int list list -> t

  val mark : int -> t -> unit

  val has_won : t -> bool

  val score : int -> t -> int option
end

val solve : int list -> Board.t list -> int

val solve_reverse : int list -> Board.t list -> int
