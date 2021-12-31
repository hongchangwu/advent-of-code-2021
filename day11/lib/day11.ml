module Coord = struct
  type t = int * int

  let compare (i1, j1) (i2, j2) =
    match compare i1 i2 with 0 -> compare j1 j2 | x -> x
end

module CoordSet = Set.Make (Coord)

let solve ~acc ~next ~terminate energymap =
  let m = Vector.size energymap in
  let n = Vector.(size (get energymap 0)) in
  (* Unfreeze the vector *)
  let energymap = Vector.(map copy energymap) in
  let neighbors (i, j) =
    List.filter
      (fun (i, j) ->
        i >= 0 && i < m && j >= 0 && j < n
        && Vector.(get (get energymap i) j) <> 0 )
      [ (i - 1, j)
      ; (i + 1, j)
      ; (i, j - 1)
      ; (i, j + 1)
      ; (i - 1, j - 1)
      ; (i - 1, j + 1)
      ; (i + 1, j - 1)
      ; (i + 1, j + 1) ]
  in
  let increment (i, j) =
    let x = Vector.(get (get energymap i) j) in
    let x' = if x = 9 then 0 else x + 1 in
    Vector.(set (get energymap i) j x') ;
    x'
  in
  let rec simulate acc i =
    match terminate acc i energymap with
    | Some x ->
      x
    | None ->
      let queue = Queue.create () in
      List.(
        iter
          (fun point -> if increment point = 0 then Queue.add point queue)
          (product (fun i j -> (i, j)) (range' 0 m) (range' 0 n))) ;
      let rec bfs visited =
        match Queue.take_opt queue with
        | None ->
          visited
        | Some point ->
          if CoordSet.mem point visited then bfs visited
          else
            let visited' = CoordSet.add point visited in
            List.iter
              (fun point -> if increment point = 0 then Queue.add point queue)
              (neighbors point) ;
            bfs visited'
      in
      let visited = bfs CoordSet.empty in
      let acc' = next acc visited in
      let i' = succ i in
      simulate acc' i'
  in
  simulate acc 0

module Part1 = struct
  let solve =
    let acc = 0 in
    let next acc visited = acc + CoordSet.cardinal visited in
    let terminate acc i _ = if i >= 100 then Some acc else None in
    solve ~acc ~next ~terminate
end

module Part2 = struct
  let solve =
    let acc = () in
    let next = Fun.const in
    let terminate () i energymap =
      if Vector.(for_all (for_all (( = ) 0)) energymap) then Some i else None
    in
    solve ~acc ~next ~terminate
end
