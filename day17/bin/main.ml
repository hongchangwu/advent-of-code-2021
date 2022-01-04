let usage = "day17 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let (x1, x2), (y1, y2) =
    let parse_line line =
      let re =
        Str.regexp
          "target[ ]area:[ ]x=\\(-?[0-9]+\\)..\\(-?[0-9]+\\),[ \
           ]y=\\(-?[0-9]+\\)..\\(-?[0-9]+\\)"
      in
      if Str.string_match re line 0 then
        let x1 = line |> Str.matched_group 1 |> int_of_string in
        let x2 = line |> Str.matched_group 2 |> int_of_string in
        let y1 = line |> Str.matched_group 3 |> int_of_string in
        let y2 = line |> Str.matched_group 4 |> int_of_string in
        ((x1, x2), (y1, y2))
      else failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          match read_line in_chan with
          | None ->
            failwith "EOF reached"
          | Some line ->
            parse_line line ))
  in
  let solve = if !part2 then Day17.Part2.solve else Day17.Part1.solve in
  Printf.printf "%d\n" (solve ~x:(x1, x2) ~y:(y1, y2))
