open Fun.Infix

module Part1 = struct
  module Frequencies = Map.Make (Int)

  let solve readings =
    let frequencies =
      let table = Hashtbl.create 32 in
      let f i = function
        | '0' ->
          Hashtbl.update table ~k:i
            ~f:(Fun.const @@ (Option.fold ( + ) (-1) %> Option.some))
        | '1' ->
          Hashtbl.update table ~k:i
            ~f:(Fun.const @@ (Option.fold ( + ) 1 %> Option.some))
        | c ->
          failwith ("Invalid digit: " ^ Char.to_string c)
      in
      List.iter (String.iteri f) readings;
      table |> Hashtbl.to_seq |> Frequencies.of_seq
    in
    let gamma, epsilon =
      Frequencies.fold
        (fun _ x (gamma, epsilon) ->
          let gamma_delta = if x > 0 then 1 else 0 in
          let epsilon_delta = if x < 0 then 1 else 0 in
          ((gamma lsl 1) + gamma_delta, (epsilon lsl 1) + epsilon_delta) )
        frequencies (0, 0)
    in
    gamma * epsilon
end

module Part2 = struct
  let int_of_binary_string =
    String.fold
      (fun acc c ->
        let delta =
          match c with
          | '0' ->
            0
          | '1' ->
            1
          | c ->
            failwith ("Invalid char: " ^ Char.to_string c)
        in
        (acc lsl 1) + delta )
      0

  let calc_oxygen_rating readings =
    let rec aux i = function
      | [reading] ->
        int_of_binary_string reading
      | readings ->
        let zeros, ones =
          List.partition (fun reading -> Char.equal reading.[i] '0') readings
        in
        if List.length zeros > List.length ones then aux (succ i) zeros
        else aux (succ i) ones
    in
    aux 0 readings

  let calc_co2_rating readings =
    let rec aux i = function
      | [reading] ->
        int_of_binary_string reading
      | readings ->
        let zeros, ones =
          List.partition (fun reading -> Char.equal reading.[i] '0') readings
        in
        if List.length zeros > List.length ones then aux (succ i) ones
        else aux (succ i) zeros
    in
    aux 0 readings

  let solve readings =
    let oxygen_rating = calc_oxygen_rating readings in
    let co2_rating = calc_co2_rating readings in
    oxygen_rating * co2_rating
end
