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
module CharSet = Set.Make (Char)
module StringMap = Map.Make (String)

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
  let solve =
    let sort_string s =
      s |> String.to_seq |> Seq.sort ~cmp:Char.compare |> String.of_seq
    in
    let get_digit s mapping =
      StringMap.get (sort_string s) mapping
      |> Option.get_exn_or ("Key error: " ^ s)
    in
    let find_mapping ss =
      let two = ref None in
      let three = ref None in
      let four = ref None in
      let six = ref [] in
      let seven = ref None in
      let to_char_set s = s |> String.to_seq |> CharSet.of_seq in
      let to_singleton = function
        | [x] ->
          x
        | _ ->
          failwith "Expected a single element"
      in
      List.iter
        (fun s ->
          match String.length s with
          | 2 ->
            two := Some (to_char_set s)
          | 3 ->
            three := Some (to_char_set s)
          | 4 ->
            four := Some (to_char_set s)
          | 5 ->
            (* We don't even need these to decode *)
            ()
          | 6 ->
            six := to_char_set s :: !six
          | 7 ->
            seven := Some (to_char_set s)
          | n ->
            failwith ("Unexpected string length: " ^ string_of_int n) )
        ss ;
      let two = !two |> Option.get_exn_or "Expected 1 pattern of length 2" in
      let three =
        !three |> Option.get_exn_or "Expected 1 pattern of length 3"
      in
      let four = !four |> Option.get_exn_or "Expected 1 pattern of length 4" in
      let six1, six2, six3 =
        match !six with
        | [x; y; z] ->
          (x, y, z)
        | _ ->
          failwith "Expected 3 patterns of length 6"
      in
      let seven =
        !seven |> Option.get_exn_or "Expected 1 pattern of length 7"
      in
      let a = CharSet.(diff three two |> to_list |> to_singleton) in
      let bd = CharSet.diff four two in
      let cf = two in
      let six1_complement =
        CharSet.(diff seven six1 |> to_list |> to_singleton)
      in
      let six2_complement =
        CharSet.(diff seven six2 |> to_list |> to_singleton)
      in
      let six3_complement =
        CharSet.(diff seven six3 |> to_list |> to_singleton)
      in
      let c, (d, e) =
        if CharSet.mem six1_complement cf then
          ( six1_complement
          , if CharSet.mem six2_complement bd then
              (six2_complement, six3_complement)
            else (six3_complement, six2_complement) )
        else if CharSet.mem six2_complement cf then
          ( six2_complement
          , if CharSet.mem six1_complement bd then
              (six1_complement, six3_complement)
            else (six3_complement, six1_complement) )
        else
          ( six3_complement
          , if CharSet.mem six1_complement bd then
              (six1_complement, six2_complement)
            else (six2_complement, six1_complement) )
      in
      let f = CharSet.(remove c cf |> to_list |> to_singleton) in
      let b = CharSet.(remove d bd |> to_list |> to_singleton) in
      let g =
        CharSet.(
          diff seven (of_list [a; b; c; d; e; f]) |> to_list |> to_singleton)
      in
      StringMap.of_list
        [ ([a; b; c; e; f; g] |> List.sort Char.compare |> String.of_list, 0)
        ; ([c; f] |> List.sort Char.compare |> String.of_list, 1)
        ; ([a; c; d; e; g] |> List.sort Char.compare |> String.of_list, 2)
        ; ([a; c; d; f; g] |> List.sort Char.compare |> String.of_list, 3)
        ; ([b; c; d; f] |> List.sort Char.compare |> String.of_list, 4)
        ; ([a; b; d; f; g] |> List.sort Char.compare |> String.of_list, 5)
        ; ([a; b; d; e; f; g] |> List.sort Char.compare |> String.of_list, 6)
        ; ([a; c; f] |> List.sort Char.compare |> String.of_list, 7)
        ; ([a; b; c; d; e; f; g] |> List.sort Char.compare |> String.of_list, 8)
        ; ([a; b; c; d; f; g] |> List.sort Char.compare |> String.of_list, 9) ]
    in
    let decode mapping =
      List.fold_left (fun acc s -> (acc * 10) + get_digit s mapping) 0
    in
    List.fold_left
      (fun acc (input, output) -> acc + decode (find_mapping input) output)
      0
end
