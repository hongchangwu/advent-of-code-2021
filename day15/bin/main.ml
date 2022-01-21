let usage = "day15 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let zero = Char.to_int '0' in
  let riskmap =
    let parse_line line =
      line |> String.to_seq
      |> Seq.map (fun c -> Char.to_int c - zero)
      |> Vector.of_seq |> Vector.freeze
    in
    IO.(
      with_in !input_file (fun in_chan ->
          in_chan |> read_lines_seq |> Seq.map parse_line |> Vector.of_seq
          |> Vector.freeze ))
  in
  let solve = if !part2 then Day15.Part2.solve else Day15.Part1.solve in
  Printf.printf "%d\n" (solve riskmap)
