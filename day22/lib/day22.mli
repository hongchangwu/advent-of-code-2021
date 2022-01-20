module Cuboid : sig
  type t = {x1 : int; x2 : int; y1 : int; y2 : int; z1 : int; z2 : int}

  val make : x1:int -> x2:int -> y1:int -> y2:int -> z1:int -> z2:int -> t
end

type action = On | Off

type instruction = {action : action; cuboid : Cuboid.t}

module Part1 : sig
  val solve : instruction list -> int
end

module Part2 : sig
  val solve : instruction list -> int
end
