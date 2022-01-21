let usage = "day19 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let beacons =
    let parse_line line =
      match String.split_on_char ',' line with
      | [x; y; z] ->
        (int_of_string x, int_of_string y, int_of_string z)
      | _ ->
        failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let rec parse_lines acc coords =
            match read_line in_chan with
            | None ->
              List.rev (List.rev coords :: acc)
            | Some "" ->
              ignore (read_line in_chan);
              parse_lines (List.rev coords :: acc) []
            | Some line ->
              let coord = parse_line line in
              parse_lines acc (coord :: coords)
          in
          ignore (read_line in_chan);
          parse_lines [] [] ))
  in
  let solve = if !part2 then Day19.Part2.solve else Day19.Part1.solve in
  Printf.printf "%d\n" (solve beacons)
