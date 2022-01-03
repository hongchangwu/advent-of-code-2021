module Pair : sig
  type t = char * char
end

module Part1 : sig
  val solve : string -> (Pair.t * char) list -> int
end

module Part2 : sig
  val solve : string -> (Pair.t * char) list -> int
end
