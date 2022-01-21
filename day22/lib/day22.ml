module Interval = struct
  type t = {low : int; high : int}

  let make low high =
    assert (low <= high);
    {low; high}

  let overlaps i1 i2 = not (i1.high < i2.low || i2.high < i1.low)

  let intersection i1 i2 =
    if overlaps i1 i2 then Some (make (max i1.low i2.low) (min i1.high i2.high))
    else None
end

module Cuboid = struct
  type t = {x1 : int; x2 : int; y1 : int; y2 : int; z1 : int; z2 : int}

  let make ~x1 ~x2 ~y1 ~y2 ~z1 ~z2 =
    assert (x1 <= x2 && y1 <= y2 && z1 <= z2);
    {x1; x2; y1; y2; z1; z2}

  let count {x1; x2; y1; y2; z1; z2} =
    (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

  let intersection c1 c2 =
    let ( let* ) = Option.bind in
    let* {low = x1; high = x2} =
      Interval.(intersection (make c1.x1 c1.x2) (make c2.x1 c2.x2))
    in
    let* {low = y1; high = y2} =
      Interval.(intersection (make c1.y1 c1.y2) (make c2.y1 c2.y2))
    in
    let* {low = z1; high = z2} =
      Interval.(intersection (make c1.z1 c1.z2) (make c2.z1 c2.z2))
    in
    Some (make ~x1 ~x2 ~y1 ~y2 ~z1 ~z2)
end

module CuboidMap = struct
  let empty = []

  let insert = List.cons

  let search (cuboid : Cuboid.t) list =
    List.filter_map (Cuboid.intersection cuboid) list

  let to_seq = List.to_seq
end

type action = On | Off

type instruction = {action : action; cuboid : Cuboid.t}

let solve ~initialization instructions =
  let step (include_map, exclude_map)
      {action; cuboid = {x1; x2; y1; y2; z1; z2} as cuboid} =
    if
      initialization
      && (x2 < -50 || x1 > 50 || y2 < -50 || y1 > 50 || z2 < -50 || z1 > 50)
    then (include_map, exclude_map)
    else
      let cuboid =
        if initialization then
          let x1 = max x1 (-50) in
          let x2 = min x2 50 in
          let y1 = max y1 (-50) in
          let y2 = min y2 50 in
          let z1 = max z1 (-50) in
          let z2 = min z2 50 in
          Cuboid.make ~x1 ~x2 ~y1 ~y2 ~z1 ~z2
        else cuboid
      in
      (* Exclude overlapping turned-on cuboids *)
      let new_excludes = CuboidMap.search cuboid include_map in
      (* Include overlapping turned-off cuboids *)
      let new_includes = CuboidMap.search cuboid exclude_map in
      (* Update include tree *)
      let include_map =
        List.fold_right CuboidMap.insert new_includes include_map
      in
      let include_map =
        match action with
        | On ->
          CuboidMap.insert cuboid include_map
        | Off ->
          include_map
      in
      (* Update exclude tree *)
      let exclude_map =
        List.fold_right CuboidMap.insert new_excludes exclude_map
      in
      (include_map, exclude_map)
  in
  let include_map, exclude_map =
    List.fold_left step (CuboidMap.empty, CuboidMap.empty) instructions
  in
  let include_count =
    include_map |> CuboidMap.to_seq |> Seq.map Cuboid.count |> Seq.fold ( + ) 0
  in
  let exclude_count =
    exclude_map |> CuboidMap.to_seq |> Seq.map Cuboid.count |> Seq.fold ( + ) 0
  in
  include_count - exclude_count

module Part1 = struct
  let solve = solve ~initialization:true
end

module Part2 = struct
  let solve = solve ~initialization:false
end
