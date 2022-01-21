let usage = "day21 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let player1_pos, player2_pos =
    IO.(
      with_in !input_file (fun in_chan ->
          let parse_pos () =
            match read_line in_chan with
            | None ->
              failwith "EOF reached"
            | Some line -> (
              match String.split ~by:": " line with
              | [_; x] ->
                int_of_string x
              | _ ->
                failwith ("Invalid line: " ^ line) )
          in
          let player1_pos = parse_pos () in
          let player2_pos = parse_pos () in
          (player1_pos, player2_pos) ))
  in
  let solve = if !part2 then Day21.Part2.solve else Day21.Part1.solve in
  Printf.printf "%d\n" (solve player1_pos player2_pos)
