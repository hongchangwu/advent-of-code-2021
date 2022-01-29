module Part1 : sig
  val solve :
    width:int ->
    height:int ->
    east_herd:(int * int) list ->
    south_herd:(int * int) list ->
    int
end
