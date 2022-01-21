let usage = "day10 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let lines = IO.(with_in !input_file read_lines_l) in
  let solve = if !part2 then Day10.Part2.solve else Day10.Part1.solve in
  Printf.printf "%d\n" (solve lines)
