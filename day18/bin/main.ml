let usage = "day18 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let trees =
    IO.(
      with_in !input_file (fun in_chan ->
          let rec parse_lines acc =
            match read_line in_chan with
            | None ->
              List.rev acc
            | Some line ->
              let tree = Day18.Tree.parse line in
              parse_lines (tree :: acc)
          in
          parse_lines [] ))
  in
  (* List.iter (Format.printf "%a\n" Day18.Tree.pp) trees ; *)
  let solve = if !part2 then Day18.Part2.solve else Day18.Part1.solve in
  Printf.printf "%d\n" (solve trees)
