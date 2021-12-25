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
