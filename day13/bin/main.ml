let usage = "day13 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let dots, folds =
    let parse_dot line =
      match String.split_on_char ',' line with
      | [x; y] ->
        (int_of_string x, int_of_string y)
      | _ ->
        failwith ("Invalid line: " ^ line)
    in
    let parse_fold line =
      let pre = "fold along " in
      if String.prefix ~pre line then
        match String.(split_on_char '=' (drop (length pre) line)) with
        | ["x"; x] ->
          `X (int_of_string x)
        | ["y"; y] ->
          `Y (int_of_string y)
        | _ ->
          failwith ("Invalid line: " ^ line)
      else failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let rec parse_dots acc =
            match read_line in_chan with
            | None ->
              failwith "EOF reached"
            | Some "" ->
              List.rev acc
            | Some line ->
              let dot = parse_dot line in
              parse_dots (dot :: acc)
          in
          let rec parse_folds acc =
            match read_line in_chan with
            | None ->
              List.rev acc
            | Some line ->
              let fold = parse_fold line in
              parse_folds (fold :: acc)
          in
          let dots = parse_dots [] |> Day13.DotSet.of_list in
          let folds = parse_folds [] in
          (dots, folds) ))
  in
  let solve = if !part2 then Day13.Part2.solve else Day13.Part1.solve in
  let dots = solve dots folds in
  Format.printf "%d\n" (Day13.DotSet.cardinal dots) ;
  Format.printf "%a\n" Day13.DotSet.pp dots
