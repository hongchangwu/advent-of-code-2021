module Packet = struct
  type t = {version: int; type_: type_}

  and type_ = Literal of int | Operator of {id: int; packets: t list}
end

let ( ** ) = Int.( ** )

let bits_of_hex_string s =
  let acc, bits =
    String.fold_right
      (fun c (acc, bits) ->
        let x =
          match c with
          | '0' ->
            [false; false; false; false]
          | '1' ->
            [false; false; false; true]
          | '2' ->
            [false; false; true; false]
          | '3' ->
            [false; false; true; true]
          | '4' ->
            [false; true; false; false]
          | '5' ->
            [false; true; false; true]
          | '6' ->
            [false; true; true; false]
          | '7' ->
            [false; true; true; true]
          | '8' ->
            [true; false; false; false]
          | '9' ->
            [true; false; false; true]
          | 'A' ->
            [true; false; true; false]
          | 'B' ->
            [true; false; true; true]
          | 'C' ->
            [true; true; false; false]
          | 'D' ->
            [true; true; false; true]
          | 'E' ->
            [true; true; true; false]
          | 'F' ->
            [true; true; true; true]
          | _ ->
            failwith ("Invalid character: " ^ Char.to_string c)
        in
        (x :: acc, bits + 4) )
      s ([], 0)
  in
  (List.concat acc, bits)

let int_of_bits =
  List.fold_left (fun acc b -> (acc lsl 1) + if b then 1 else 0) 0

let four_bit_mask = (2 ** 4) - 1

let parse_literal bits size =
  let rec loop acc bits size =
    let chunk, bits = List.take_drop 5 bits in
    let size = size - 5 in
    let y = int_of_bits chunk in
    let x = y land four_bit_mask in
    let acc = (acc lsl 4) + x in
    if y lsr 4 = 1 then loop acc bits size else (acc, bits, size)
  in
  loop 0 bits size

let rec parse_multi_by_total_bits length bits size =
  let target = size - length in
  let rec loop acc bits size =
    if size <= target then (List.rev acc, bits, size)
    else
      let packet, bits, size = parse_one bits size in
      loop (packet :: acc) bits size
  in
  loop [] bits size

and parse_multi_by_num_packets length bits size =
  let rec loop i acc bits size =
    if i >= length then (List.rev acc, bits, size)
    else
      let packet, bits, size = parse_one bits size in
      loop (succ i) (packet :: acc) bits size
  in
  loop 0 [] bits size

and parse_one bits size : Packet.t * bool list * int =
  let chunk, bits = List.take_drop 3 bits in
  let size = size - 3 in
  let version = int_of_bits chunk in
  let chunk, bits = List.take_drop 3 bits in
  let size = size - 3 in
  let type_id = int_of_bits chunk in
  if type_id = 4 then
    let x, bits, size = parse_literal bits size in
    ({version; type_= Literal x}, bits, size)
  else
    let chunk, bits = List.take_drop 1 bits in
    let size = size - 1 in
    let length_type = int_of_bits chunk in
    if length_type = 0 then
      let chunk, bits = List.take_drop 15 bits in
      let size = size - 15 in
      let length = int_of_bits chunk in
      let packets, bits, size = parse_multi_by_total_bits length bits size in
      ({version; type_= Operator {id= type_id; packets}}, bits, size)
    else if length_type = 1 then
      let chunk, bits = List.take_drop 11 bits in
      let size = size - 11 in
      let length = int_of_bits chunk in
      let packets, bits, size = parse_multi_by_num_packets length bits size in
      ({version; type_= Operator {id= type_id; packets}}, bits, size)
    else failwith ("Invalid length type: " ^ string_of_int length_type)

module Part1 = struct
  let solve input =
    let bits, size = bits_of_hex_string input in
    let packet, _, _ = parse_one bits size in
    let rec calc acc ({version; type_} : Packet.t) =
      let acc = acc + version in
      match type_ with
      | Literal _ ->
        acc
      | Operator {packets; _} ->
        List.fold_left calc acc packets
    in
    calc 0 packet
end

module Part2 = struct
  let solve input =
    let bits, size = bits_of_hex_string input in
    let packet, _, _ = parse_one bits size in
    let rec eval ({type_; _} : Packet.t) =
      match type_ with
      | Literal x ->
        x
      | Operator {id; packets} -> (
        match id with
        | 0 ->
          (* sum *)
          List.fold_left (fun acc p -> acc + eval p) 0 packets
        | 1 ->
          (* product *)
          List.fold_left (fun acc p -> acc * eval p) 1 packets
        | 2 ->
          (* minimum *)
          List.fold_left (fun acc p -> min acc (eval p)) Int.max_int packets
        | 3 ->
          (* maximum *)
          List.fold_left (fun acc p -> max acc (eval p)) Int.min_int packets
        | 5 -> (
          (* greater than *)
          match packets with
          | [first; second] ->
            let x = eval first in
            let y = eval second in
            if x > y then 1 else 0
          | _ ->
            failwith "Expected 2 sub-packets exactly" )
        | 6 -> (
          (* less than *)
          match packets with
          | [first; second] ->
            let x = eval first in
            let y = eval second in
            if x < y then 1 else 0
          | _ ->
            failwith "Expected 2 sub-packets exactly" )
        | 7 -> (
          (* equal to *)
          match packets with
          | [first; second] ->
            let x = eval first in
            let y = eval second in
            if x = y then 1 else 0
          | _ ->
            failwith "Expected 2 sub-packets exactly" )
        | _ ->
          failwith ("Invalid type ID: " ^ string_of_int id) )
    in
    eval packet
end
