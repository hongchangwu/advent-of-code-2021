let usage = "day02 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let parse_line line =
  match String.split_on_char ' ' line with
  | "forward" :: x :: _ ->
    (`Forward, int_of_string x)
  | "up" :: x :: _ ->
    (`Up, int_of_string x)
  | "down" :: x :: _ ->
    (`Down, int_of_string x)
  | _ ->
    failwith ("Invalid line: " ^ line)

let () =
  Arg.parse specs anon usage;
  let directions =
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          lines |> Seq.map parse_line |> Seq.to_list ))
  in
  let solve = if !part2 then Day02.Part2.solve else Day02.Part1.solve in
  Printf.printf "%d\n" (solve directions)
