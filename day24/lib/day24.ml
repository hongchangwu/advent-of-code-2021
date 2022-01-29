type var = W | X | Y | Z

type atom = Var of var | Val of int

type instruction =
  | Inp of var
  | Add of (var * atom)
  | Mul of (var * atom)
  | Div of (var * atom)
  | Mod of (var * atom)
  | Eql of (var * atom)

(* The given instruction sets take two basic forms *)
type reduced_form =
  (* x = z % 26 + a != w
     y = (w + b) * x
     z = z * (25 * x + 1) + (w + b) * x *)
  | Form1 of (int * int)
  (* x = z % 26 - a != w
     y = (w + b) * x
     z = z / 26 * (25 * x + 1) + (w + b) * x *)
  | Form2 of (int * int)

let reduce_instructions instructions =
  let instructions = Array.of_list instructions in
  let b =
    match instructions.(15) with
    | Add (_, Val b) ->
      b
    | _ ->
      failwith "Invalid instructions"
  in
  match instructions.(5) with
  | Add (_, Val a) when a > 0 ->
    Form1 (a, b)
  | Add (_, Val a) when a < 0 ->
    Form2 (-a, b)
  | _ ->
    failwith "Invalid instructions"

let digits_to_decimal = List.fold_left (fun acc x -> (10 * acc) + x) 0

module IntSet = Set.Make (Int)

let solve instructions =
  let instructions =
    instructions |> List.chunks 18 |> List.map reduce_instructions
  in
  assert (List.length instructions = 14);
  (* Reverse search from the last digit *)
  let rec rev_search acc digits z = function
    | [] ->
      if z = 0 then digits :: acc else acc
    | instruction :: instructions ->
      let next =
        match instruction with
        | Form1 (_, b) ->
          (* x = 1 *)
          let z' = z / 26 in
          let w = z - (26 * z') - b in
          if w >= 1 && w <= 9 then [(w, z')] else []
        | Form2 (a, b) ->
          (* x = 1 *)
          let d = z / 26 in
          let w = z - (26 * d) - b in
          let next1 =
            if w >= 1 && w <= 9 then
              List.range' (26 * d) (26 * (d + 1))
              |> List.filter_map (fun z ->
                     if (z mod 26) - a = w then None else Some (w, z) )
            else []
          in
          (* x = 0 *)
          let next2 =
            List.range' (26 * z) (26 * (z + 1))
            |> List.flat_map (fun z ->
                   List.filter_map
                     (fun w -> if (z mod 26) - a = w then Some (w, z) else None)
                     (List.range 1 9) )
          in
          next1 @ next2
      in
      List.fold_left
        (fun acc (w, z) -> rev_search acc (w :: digits) z instructions)
        acc next
  in
  instructions |> List.rev |> rev_search [] [] 0 |> List.map digits_to_decimal
  |> IntSet.of_list

module Part1 = struct
  let solve instructions = instructions |> solve |> IntSet.max_elt
end

module Part2 = struct
  let solve instructions = instructions |> solve |> IntSet.min_elt
end
