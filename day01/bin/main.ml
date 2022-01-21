let usage = "day01 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let numbers =
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          lines |> Seq.map int_of_string |> Seq.to_list ))
  in
  let solve = if !part2 then Day01.Part2.solve else Day01.Part1.solve in
  Printf.printf "%d\n" (solve numbers)
