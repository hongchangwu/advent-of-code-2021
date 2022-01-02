type fold = [`X of int | `Y of int]

module Dot : sig
  type t = int * int
end

module DotSet : sig
  include Set.S with type elt = Dot.t

  val pp : Format.formatter -> t -> unit
end

module Part1 : sig
  val solve : DotSet.t -> [< fold] list -> DotSet.t
end

module Part2 : sig
  val solve : DotSet.t -> [< fold] list -> DotSet.t
end
