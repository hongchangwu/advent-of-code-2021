open Fun.Infix

module Coord = struct
  type t = int * int * int

  let compare (x1, y1, z1) (x2, y2, z2) =
    match compare x1 x2 with
    | 0 -> (
      match compare y1 y2 with 0 -> compare z1 z2 | ord -> ord )
    | ord ->
      ord
end

module CoordSet = Set.Make (Coord)

let rotations coords =
  let mappers =
    [ Fun.id;
      (fun (x, y, z) -> (x, z, -y));
      (fun (x, y, z) -> (x, -y, -z));
      (fun (x, y, z) -> (x, -z, y));
      (fun (x, y, z) -> (-x, -y, z));
      (fun (x, y, z) -> (-x, -z, -y));
      (fun (x, y, z) -> (-x, y, -z));
      (fun (x, y, z) -> (-x, z, y));
      (fun (x, y, z) -> (-y, x, z));
      (fun (x, y, z) -> (-z, x, -y));
      (fun (x, y, z) -> (y, x, -z));
      (fun (x, y, z) -> (z, x, y));
      (fun (x, y, z) -> (y, -x, z));
      (fun (x, y, z) -> (z, -x, -y));
      (fun (x, y, z) -> (-y, -x, -z));
      (fun (x, y, z) -> (-z, -x, y));
      (fun (x, y, z) -> (y, z, x));
      (fun (x, y, z) -> (z, -y, x));
      (fun (x, y, z) -> (-y, -z, x));
      (fun (x, y, z) -> (-z, y, x));
      (fun (x, y, z) -> (-y, z, -x));
      (fun (x, y, z) -> (-z, -y, -x));
      (fun (x, y, z) -> (y, -z, -x));
      (fun (x, y, z) -> (z, y, -x)) ]
  in
  mappers |> Seq.of_list |> Seq.map (Fun.flip List.map coords)

let check (_, beaconSet) beacons =
  let beaconSeq = CoordSet.to_seq beaconSet in
  let check_rotation beacons =
    let check_pair (x1, y1, z1) (x2, y2, z2) =
      let dx = x2 - x1 in
      let dy = y2 - y1 in
      let dz = z2 - z1 in
      let beaconSet' =
        List.map (fun (x, y, z) -> (x - dx, y - dy, z - dz)) beacons
        |> CoordSet.of_list
      in
      if CoordSet.(inter beaconSet beaconSet' |> cardinal) >= 12 then
        Some ((-dx, -dy, -dz), beaconSet')
      else None
    in
    let rec check_rotation' = function
      | [] ->
        None
      | beacon :: beacons' ->
        beaconSeq
        |> Seq.filter_map (Fun.flip check_pair beacon)
        |> Seq.head
        |> Option.or_lazy ~else_:(fun () -> check_rotation' beacons')
    in
    check_rotation' beacons
  in
  beacons |> rotations |> Seq.filter_map check_rotation |> Seq.head

let solve = function
  | [] ->
    failwith "Empty"
  | first :: rest ->
    let knowns = [((0, 0, 0), CoordSet.of_list first)] in
    let unknowns = rest in
    let rec loop knowns = function
      | [] ->
        knowns
      | beacons :: unknowns -> (
        let rec inner_loop = function
          | [] ->
            None
          | first :: rest ->
            check first beacons
            |> Option.or_lazy ~else_:(fun () -> inner_loop rest)
        in
        match inner_loop knowns with
        | None ->
          loop knowns (unknowns @ [beacons])
        | Some known ->
          loop (known :: knowns) unknowns )
    in
    loop knowns unknowns

let manhattan_distance (x1, y1, z1) (x2, y2, z2) =
  abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

module Part1 = struct
  let solve =
    solve
    %> List.fold_left (fun acc (_, s) -> CoordSet.union acc s) CoordSet.empty
    %> CoordSet.cardinal
end

module Part2 = struct
  let solve inputs =
    let scanners = solve inputs |> List.map fst |> Array.of_list in
    let n = Array.length scanners in
    let max_distance = ref 0 in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        let distance = manhattan_distance scanners.(i) scanners.(j) in
        max_distance := max !max_distance distance
      done
    done;
    !max_distance
end
