let usage = "day16 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let input =
    IO.(
      with_in !input_file (fun in_chan ->
          match read_line in_chan with
          | None ->
            failwith "EOF reached"
          | Some line ->
            line ))
  in
  let solve = if !part2 then Day16.Part2.solve else Day16.Part1.solve in
  Printf.printf "%d\n" (solve input)
