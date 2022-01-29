module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with 0 -> compare y1 y2 | ord -> ord
end

module CoordSet = Set.Make (Coord)

module Part1 = struct
  let solve ~width ~height ~east_herd ~south_herd =
    let east_herd = CoordSet.of_list east_herd in
    let south_herd = CoordSet.of_list south_herd in
    let move_east east_herd south_herd =
      let change_list =
        east_herd |> CoordSet.to_seq
        |> Seq.filter_map (fun (x, y) ->
               let y' = (y + 1) mod width in
               if
                 CoordSet.mem (x, y') east_herd
                 || CoordSet.mem (x, y') south_herd
               then None
               else Some ((x, y), (x, y')) )
        |> Seq.to_list
      in
      let east_herd =
        List.fold_left
          (fun east_herd ((x1, y1), (x2, y2)) ->
            east_herd |> CoordSet.remove (x1, y1) |> CoordSet.add (x2, y2) )
          east_herd change_list
      in
      (east_herd, List.length change_list)
    in
    let move_south east_herd south_herd =
      let change_list =
        south_herd |> CoordSet.to_seq
        |> Seq.filter_map (fun (x, y) ->
               let x' = (x + 1) mod height in
               if
                 CoordSet.mem (x', y) east_herd
                 || CoordSet.mem (x', y) south_herd
               then None
               else Some ((x, y), (x', y)) )
        |> Seq.to_list
      in
      let south_herd =
        List.fold_left
          (fun south_herd ((x1, y1), (x2, y2)) ->
            south_herd |> CoordSet.remove (x1, y1) |> CoordSet.add (x2, y2) )
          south_herd change_list
      in
      (south_herd, List.length change_list)
    in
    let rec loop steps east_herd south_herd =
      let east_herd, east_herd_changes = move_east east_herd south_herd in
      let south_herd, south_herd_changes = move_south east_herd south_herd in
      if east_herd_changes + south_herd_changes = 0 then steps
      else loop (succ steps) east_herd south_herd
    in
    loop 1 east_herd south_herd
end
