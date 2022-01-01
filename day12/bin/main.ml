let usage = "day12 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let edges =
    let parse_line line =
      match String.split_on_char '-' line with
      | [a; b] ->
        (a, b)
      | _ ->
        failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          in_chan |> read_lines_seq |> Seq.map parse_line |> Seq.to_list ))
  in
  let solve = if !part2 then Day12.Part2.solve else Day12.Part1.solve in
  Printf.printf "%d\n" (solve edges)
