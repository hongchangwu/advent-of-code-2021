module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with 0 -> compare y1 y2 | ord -> ord
end

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)

module ImageMap = struct
  type t = {pixels : bool CoordMap.t; default : bool}

  let make pixels default = {pixels; default}

  let get k {pixels; default} = CoordMap.get_or k pixels ~default
end

let to_binary_num =
  List.fold_left (fun acc b -> (acc lsl 1) + if b then 1 else 0) 0

let neighbors (x, y) =
  [ (x - 1, y - 1)
  ; (x - 1, y)
  ; (x - 1, y + 1)
  ; (x, y - 1)
  ; (x, y)
  ; (x, y + 1)
  ; (x + 1, y - 1)
  ; (x + 1, y)
  ; (x + 1, y + 1) ]

let enhance_pixel enhancement image_map coord =
  let idx =
    coord |> neighbors
    |> List.map (Fun.flip ImageMap.get image_map)
    |> to_binary_num
  in
  Vector.get enhancement idx

let enhance enhancement ({pixels; default} as image_map : ImageMap.t) =
  let enhanced = CoordSet.empty in
  let pixels, _ =
    List.fold_left
      (fun (pixels, enhanced) (coord, _) ->
        let coords =
          coord |> neighbors
          |> List.filter (Fun.negate (Fun.flip CoordSet.mem enhanced))
        in
        let pixels =
          List.fold_left
            (fun acc k ->
              CoordMap.add k (enhance_pixel enhancement image_map k) acc )
            pixels coords
        in
        let enhanced = List.fold_left (Fun.flip CoordSet.add) enhanced coords in
        (pixels, enhanced) )
      (pixels, enhanced) (CoordMap.to_list pixels)
  in
  let default =
    Vector.get enhancement (to_binary_num (List.init 9 (Fun.const default)))
  in
  ImageMap.make pixels default

let solve n enhancement light_pixels =
  let light_pixels =
    List.fold_left
      (fun acc k -> CoordMap.add k true acc)
      CoordMap.empty light_pixels
  in
  let image_map = ref (ImageMap.make light_pixels false) in
  let enhance = enhance enhancement in
  for _ = 1 to n do
    image_map := enhance !image_map
  done ;
  let ImageMap.{pixels; _} = !image_map in
  pixels |> CoordMap.to_seq |> Seq.filter snd |> Seq.length

module Part1 = struct
  let solve = solve 2
end

module Part2 = struct
  let solve = solve 50
end
