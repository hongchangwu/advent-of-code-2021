type var = W | X | Y | Z

type atom = Var of var | Val of int

type instruction =
  | Inp of var
  | Add of (var * atom)
  | Mul of (var * atom)
  | Div of (var * atom)
  | Mod of (var * atom)
  | Eql of (var * atom)

(* type state = {w : int; x : int; y : int; z : int} *)

(* val pp_state : Format.formatter -> state -> unit *)

(* val execute : int list -> instruction list -> (state, string) result *)

module Part1 : sig
  val solve : instruction list -> int
end

module Part2 : sig
  val solve : instruction list -> int
end
