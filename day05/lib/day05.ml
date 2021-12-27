module Interval_map = Interval_map.Make (Int)

let sgn = function 0 -> 0 | x when x > 0 -> 1 | _ -> -1

let solve ~diagonal lines =
  let points = Hashtbl.create 32 in
  let rows = Hashtbl.create 100 in
  let columns = Hashtbl.create 100 in
  (* e.g., [(0, 0); (1, 1); (2; 2)]*)
  let diagonals1 = Hashtbl.create 100 in
  (* e.g., [(0, 2); (1, 1); (2; 0)]*)
  let diagonals2 = Hashtbl.create 100 in
  let insert_and_update target i x y make_point =
    let low, high = if x <= y then (x, y) else (y, x) in
    let interval =
      Interval_map.Interval.create (Included low) (Included high)
    in
    match Hashtbl.get target i with
    | None ->
      Hashtbl.add target i Interval_map.(empty |> add interval ())
    | Some ivm ->
      ivm
      |> Interval_map.query_interval_list interval
      |> List.iter (fun (ivl, _) ->
             match Interval_map.Interval.overlap_interval interval ivl with
             | Some {low= Included low; high= Included high} ->
               List.range low high
               |> List.iter (fun j -> Hashtbl.replace points (make_point j) ())
             | _ ->
               () ) ;
      let ivm = Interval_map.add interval () ivm in
      Hashtbl.replace target i ivm
  in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      if x1 = x2 then (
        let x = x1 in
        insert_and_update rows x y1 y2 (fun y -> (x, y)) ;
        List.range y1 y2
        |> List.iter (fun y ->
               insert_and_update columns y x x (fun x -> (x, y)) ;
               if diagonal then (
                 let d = y - x in
                 let s = x + y in
                 insert_and_update diagonals1 d x x (fun x -> (x, x + d)) ;
                 insert_and_update diagonals2 s x x (fun x -> (x, s - x)) ) ) )
      else if y1 = y2 then (
        let y = y1 in
        insert_and_update columns y x1 x2 (fun x -> (x, y)) ;
        List.range x1 x2
        |> List.iter (fun x ->
               insert_and_update rows x y y (fun y -> (x, y)) ;
               if diagonal then (
                 let d = y - x in
                 let s = x + y in
                 insert_and_update diagonals1 (y - x) x x (fun x -> (x, x + d)) ;
                 insert_and_update diagonals2 (x + y) x x (fun x -> (x, s - x))
                 ) ) )
      else if diagonal then (
        if sgn (x2 - x1) = sgn (y2 - y1) then (
          let d = y1 - x1 in
          insert_and_update diagonals1 d x1 x2 (fun x -> (x, x + d)) ;
          List.range x1 x2
          |> List.iter (fun x ->
                 let y = x + d in
                 let s = x + y in
                 insert_and_update rows x y y (fun y -> (x, y)) ;
                 insert_and_update columns y x x (fun x -> (x, y)) ;
                 insert_and_update diagonals2 s x x (fun x -> (x, s - x)) ) )
        else
          let s = x1 + y1 in
          insert_and_update diagonals2 s x1 x2 (fun x -> (x, s - x)) ;
          List.range x1 x2
          |> List.iter (fun x ->
                 let y = s - x in
                 let d = y - x in
                 insert_and_update rows x y y (fun y -> (x, y)) ;
                 insert_and_update columns y x x (fun x -> (x, y)) ;
                 insert_and_update diagonals1 d x x (fun x -> (x, x + d)) ) ) )
    lines ;
  Hashtbl.length points
