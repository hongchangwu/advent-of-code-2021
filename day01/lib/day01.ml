open Containers

let rec pairs = function
  | [] | [_] ->
    []
  | x :: (y :: _ as res) ->
    (x, y) :: pairs res

let sum = List.fold_left (+) 0

let sliding n xs =
  let rec aux k = function
    | _ when k < n -> []
    | xs -> List.take n xs :: aux (pred k) (List.drop 1 xs)
  in
  aux (List.length xs) xs

let solve ?window numbers =
  let aux f xs =
    List.fold_left
      (fun count (x, y) -> if f y > f x then count + 1 else count)
      0 (pairs xs)
  in
  match window with
  | None -> aux Fun.id numbers
  | Some n -> aux sum (sliding n numbers)
