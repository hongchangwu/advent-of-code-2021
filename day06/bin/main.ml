let usage = "day06 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let timers =
    IO.(
      with_in !input_file (fun in_chan ->
          read_line in_chan
          |> Option.get_exn_or "EOF reached"
          |> String.split_on_char ',' |> List.map int_of_string ))
  in
  let solve = if !part2 then Day06.Part2.solve else Day06.Part1.solve in
  Printf.printf "%d\n" (solve timers)
