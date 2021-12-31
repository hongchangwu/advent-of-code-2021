module Coord = struct
  type t = int * int

  let compare (i1, j1) (i2, j2) =
    match compare i1 i2 with 0 -> compare j1 j2 | x -> x
end

module CoordSet = Set.Make (Coord)
module IntMap = Map.Make (Int)

module Part1 = struct
  let solve heightmap =
    let m = Vector.size heightmap in
    let n = Vector.(size (get heightmap 0)) in
    let neighbors (i, j) =
      let x = Vector.(get (get heightmap i) j) in
      List.filter
        (fun (i, j) ->
          i >= 0 && i < m && j >= 0 && j < n
          && Vector.(get (get heightmap i) j) <= x )
        [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
    in
    let rec dfs (visited, lowpoints) point =
      if CoordSet.mem point visited then (visited, lowpoints)
      else
        let visited' = CoordSet.add point visited in
        match neighbors point with
        | [] ->
          (visited', CoordSet.add point lowpoints)
        | points ->
          List.fold_left dfs (visited', lowpoints) points
    in
    let _, lowpoints =
      List.(
        fold_left dfs
          (CoordSet.empty, CoordSet.empty)
          (product (fun i j -> (i, j)) (range' 0 m) (range' 0 n)))
    in
    lowpoints |> CoordSet.to_seq
    |> Seq.fold (fun acc (i, j) -> acc + Vector.(get (get heightmap i) j) + 1) 0
end

module Part2 = struct
  let solve heightmap =
    let m = Vector.size heightmap in
    let n = Vector.(size (get heightmap 0)) in
    let neighbors (i, j) =
      List.filter
        (fun (i, j) ->
          i >= 0 && i < m && j >= 0 && j < n
          && Vector.(get (get heightmap i) j) <> 9 )
        [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
    in
    let rec dfs (visited, key, basins) point =
      let visited' = CoordSet.add point visited in
      let basins' =
        IntMap.update key
          (function None -> Some 1 | Some x -> Some (x + 1))
          basins
      in
      match neighbors point with
      | [] ->
        (visited', key, basins')
      | points ->
        List.fold_left
          (fun ((visited, _, _) as acc) point ->
            if CoordSet.mem point visited then acc else dfs acc point )
          (visited', key, basins') points
    in
    let _, _, basins =
      List.(
        fold_left
          (fun ((visited, _, _) as acc) ((i, j) as point) ->
            if
              CoordSet.mem point visited || Vector.(get (get heightmap i) j) = 9
            then acc
            else
              let visited, key, basins = dfs acc point in
              (visited, key + 1, basins) )
          (CoordSet.empty, 0, IntMap.empty)
          (product (fun i j -> (i, j)) (range' 0 m) (range' 0 n)))
    in
    basins |> IntMap.to_seq |> Seq.map snd
    |> Seq.sort ~cmp:(Fun.flip Int.compare)
    |> Seq.take 3 |> Seq.fold ( * ) 1
end
