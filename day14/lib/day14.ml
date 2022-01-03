open Fun.Infix

module Pair = struct
  type t = char * char

  let compare (x1, y1) (x2, y2) =
    match Char.compare x1 x2 with 0 -> Char.compare y1 y2 | ord -> ord
end

module PairMap = Map.Make (Pair)
module CharMap = Map.Make (Char)

let count_frequencies (head, _) pairs =
  let map =
    CharMap.of_list_with ~f:(Fun.const ( + ))
      ( (head, 1)
      :: ( pairs |> PairMap.to_seq
         |> Seq.map (fun ((_, y), c) -> (y, c))
         |> Seq.to_list ) )
  in
  map |> CharMap.to_seq
  |> Seq.sort ~cmp:(fun (_, c1) (_, c2) -> compare c1 c2)
  |> Seq.to_list

let solve steps template rules =
  let rules =
    rules |> List.to_seq
    |> Seq.map (fun ((x, y), z) -> ((x, y), ((x, z), (z, y))))
    |> PairMap.of_seq
  in
  let head = String.(get template 0, get template 1) in
  let chars = String.to_seq template in
  let pairs =
    Seq.(zip chars (drop 1 chars))
    |> Seq.map (fun x -> (x, 1))
    |> PairMap.of_seq_with ~f:(Fun.const ( + ))
  in
  let rec insert head pairs i =
    if i >= steps then (head, pairs)
    else
      let pairs =
        PairMap.fold
          (fun pair c acc ->
            match PairMap.get pair rules with
            | None ->
              acc
            | Some (pair1, pair2) ->
              acc
              |> PairMap.update pair (Option.map (Fun.flip ( - ) c))
              |> PairMap.update pair1
                   (Option.map (( + ) c) %> Option.or_ ~else_:(Some c))
              |> PairMap.update pair2
                   (Option.map (( + ) c) %> Option.or_ ~else_:(Some c)) )
          pairs pairs
      in
      let head =
        match PairMap.get head rules with None -> head | Some (x, _) -> x
      in
      insert head pairs (succ i)
  in
  let head, pairs = insert head pairs 0 in
  let frequencies = count_frequencies head pairs in
  let high = frequencies |> List.last_opt |> Option.fold (Fun.const snd) 0 in
  let low = frequencies |> List.head_opt |> Option.fold (Fun.const snd) 0 in
  high - low

module Part1 = struct
  let solve = solve 10
end

module Part2 = struct
  let solve = solve 40
end
