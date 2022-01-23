let usage = "day23 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let input =
    let amphipod_of_string : string -> Day23.amphipod = function
      | "A" ->
        Amber
      | "B" ->
        Bronze
      | "C" ->
        Copper
      | "D" ->
        Desert
      | s ->
        failwith ("invalid input: " ^ s)
    in
    let parse_top_row line =
      let re =
        Str.regexp {|^###\([ABCD]\)#\([ABCD]\)#\([ABCD]\)#\([ABCD]\)###$|}
      in
      if Str.string_match re line 0 then
        let a1 = line |> Str.matched_group 1 |> amphipod_of_string in
        let a2 = line |> Str.matched_group 2 |> amphipod_of_string in
        let a3 = line |> Str.matched_group 3 |> amphipod_of_string in
        let a4 = line |> Str.matched_group 4 |> amphipod_of_string in
        (a1, a2, a3, a4)
      else failwith ("Invalid line: " ^ line)
    in
    let parse_bottom_row line =
      let re =
        Str.regexp {|^[ ][ ]#\([ABCD]\)#\([ABCD]\)#\([ABCD]\)#\([ABCD]\)#$|}
      in
      if Str.string_match re line 0 then
        let a1 = line |> Str.matched_group 1 |> amphipod_of_string in
        let a2 = line |> Str.matched_group 2 |> amphipod_of_string in
        let a3 = line |> Str.matched_group 3 |> amphipod_of_string in
        let a4 = line |> Str.matched_group 4 |> amphipod_of_string in
        (a1, a2, a3, a4)
      else failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          ignore (read_line in_chan);
          ignore (read_line in_chan);
          let a11, a12, a13, a14 =
            match read_line in_chan with
            | Some line ->
              parse_top_row line
            | None ->
              failwith "EOF reached"
          in
          let a21, a22, a23, a24 =
            match read_line in_chan with
            | Some line ->
              parse_bottom_row line
            | None ->
              failwith "EOF reached"
          in
          ((a11, a21), (a12, a22), (a13, a23), (a14, a24)) ))
  in
  let solve = if !part2 then Day23.Part2.solve else Day23.Part1.solve in
  Printf.printf "%d\n" (solve input)
