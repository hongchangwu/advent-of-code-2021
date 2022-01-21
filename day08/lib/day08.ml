(*   0:      1:      2:      3:      4: *)
(*  aaaa    ....    aaaa    aaaa    .... *)
(* b    c  .    c  .    c  .    c  b    c *)
(* b    c  .    c  .    c  .    c  b    c *)
(*  ....    ....    dddd    dddd    dddd *)
(* e    f  .    f  e    .  .    f  .    f *)
(* e    f  .    f  e    .  .    f  .    f *)
(*  gggg    ....    gggg    gggg    .... *)

(*   5:      6:      7:      8:      9: *)
(*  aaaa    aaaa    aaaa    aaaa    aaaa *)
(* b    .  b    .  .    c  b    c  b    c *)
(* b    .  b    .  .    c  b    c  b    c *)
(*  dddd    dddd    ....    dddd    dddd *)
(* .    f  e    f  .    f  e    f  .    f *)
(* .    f  e    f  .    f  e    f  .    f *)
(*  gggg    gggg    ....    gggg    gggg *)

(* Number of unique segments: *)
(* 0: 6 *)
(* 1: 2 *)
(* 2: 5 *)
(* 3: 5 *)
(* 4: 4 *)
(* 5: 5 *)
(* 6: 6 *)
(* 7: 3 *)
(* 8: 7 *)
(* 9: 6 *)

open Fun.Infix

module Part1 = struct
  let solve entries =
    let is_unique s =
      let n = String.length s in
      n = 2 || n = 3 || n = 4 || n = 7
    in
    let count_unique = List.filter is_unique %> List.length in
    List.fold_left (fun acc (_, output) -> acc + count_unique output) 0 entries
end

module Part2 = struct
  module CharSet = Set.Make (Char)
  module StringMap = Map.Make (String)

  let solve =
    let sort_string s =
      s |> String.to_seq |> Seq.sort ~cmp:Char.compare |> String.of_seq
    in
    let get_digit s mapping =
      StringMap.get (sort_string s) mapping
      |> Option.get_exn_or ("Key error: " ^ s)
    in
    let find_mapping ss =
      let two = ref [] in
      let three = ref [] in
      let four = ref [] in
      let six = ref [] in
      let seven = ref [] in
      let to_char_set s = s |> String.to_seq |> CharSet.of_seq in
      let to_singleton set =
        set |> CharSet.to_list
        |> function [x] -> x | _ -> failwith "Expected a single element"
      in
      List.iter
        (fun s ->
          match String.length s with
          | 2 ->
            two := to_char_set s :: !two
          | 3 ->
            three := to_char_set s :: !three
          | 4 ->
            four := to_char_set s :: !four
          | 5 ->
            (* We don't even need these to decode *)
            ()
          | 6 ->
            six := to_char_set s :: !six
          | 7 ->
            seven := to_char_set s :: !seven
          | n ->
            failwith ("Unexpected string length: " ^ string_of_int n) )
        ss;
      let two =
        match !two with
        | [x] ->
          x
        | _ ->
          failwith "Expected 1 pattern of length 2"
      in
      let three =
        match !three with
        | [x] ->
          x
        | _ ->
          failwith "Expected 1 pattern of length 3"
      in
      let four =
        match !four with
        | [x] ->
          x
        | _ ->
          failwith "Expected 1 pattern of length 4"
      in
      let six1, six2, six3 =
        match !six with
        | [x; y; z] ->
          (x, y, z)
        | _ ->
          failwith "Expected 3 patterns of length 6"
      in
      let seven =
        match !seven with
        | [x] ->
          x
        | _ ->
          failwith "Expected 1 pattern of length 7"
      in
      let a = CharSet.diff three two |> to_singleton in
      let bd = CharSet.diff four two in
      let cf = two in
      let six1_complement = CharSet.diff seven six1 |> to_singleton in
      let six2_complement = CharSet.diff seven six2 |> to_singleton in
      let six3_complement = CharSet.diff seven six3 |> to_singleton in
      let c, (d, e) =
        if CharSet.mem six1_complement cf then
          ( six1_complement,
            if CharSet.mem six2_complement bd then
              (six2_complement, six3_complement)
            else (six3_complement, six2_complement) )
        else if CharSet.mem six2_complement cf then
          ( six2_complement,
            if CharSet.mem six1_complement bd then
              (six1_complement, six3_complement)
            else (six3_complement, six1_complement) )
        else
          ( six3_complement,
            if CharSet.mem six1_complement bd then
              (six1_complement, six2_complement)
            else (six2_complement, six1_complement) )
      in
      let f = CharSet.remove c cf |> to_singleton in
      let b = CharSet.remove d bd |> to_singleton in
      let g =
        CharSet.(diff seven (of_list [a; b; c; d; e; f]) |> to_singleton)
      in
      let to_key = List.sort Char.compare %> String.of_list in
      StringMap.of_list
        [ (to_key [a; b; c; e; f; g], 0);
          (to_key [c; f], 1);
          (to_key [a; c; d; e; g], 2);
          (to_key [a; c; d; f; g], 3);
          (to_key [b; c; d; f], 4);
          (to_key [a; b; d; f; g], 5);
          (to_key [a; b; d; e; f; g], 6);
          (to_key [a; c; f], 7);
          (to_key [a; b; c; d; e; f; g], 8);
          (to_key [a; b; c; d; f; g], 9) ]
    in
    let decode mapping =
      List.fold_left (fun acc s -> (acc * 10) + get_digit s mapping) 0
    in
    List.fold_left
      (fun acc (input, output) -> acc + decode (find_mapping input) output)
      0
end
