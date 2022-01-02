type fold = [`X of int | `Y of int]

module Dot = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with 0 -> compare y1 y2 | ord -> ord
end

module DotSet = struct
  include Set.Make (Dot)

  let pp fmt dots =
    let m, n = fold (fun (x, y) (m, n) -> (max x m, max y n)) dots (0, 0) in
    List.iter
      (fun y ->
        List.iter
          (fun x ->
            if mem (x, y) dots then Format.char fmt '#' else Format.char fmt '.'
            )
          (List.range 0 m) ;
        Format.newline fmt () )
      (List.range 0 n)
end

let fold_x dots x0 =
  List.fold_left
    (fun dots (x, y) ->
      if x = x0 then DotSet.remove (x, y) dots
      else if x > x0 then
        dots |> DotSet.remove (x, y) |> DotSet.add ((2 * x0) - x, y)
      else dots )
    dots (DotSet.elements dots)

let fold_y dots y0 =
  List.fold_left
    (fun dots (x, y) ->
      if y = y0 then DotSet.remove (x, y) dots
      else if y > y0 then
        dots |> DotSet.remove (x, y) |> DotSet.add (x, (2 * y0) - y)
      else dots )
    dots (DotSet.elements dots)

let fold dots = function `X x -> fold_x dots x | `Y y -> fold_y dots y

module Part1 = struct
  let solve dots folds = fold dots (List.hd folds)
end

module Part2 = struct
  let solve dots folds = List.fold_left fold dots folds
end
