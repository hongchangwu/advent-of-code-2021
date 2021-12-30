let usage = "day05 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let lines =
    let open IO in
    with_in !input_file (fun in_chan ->
        let parse_pos pos =
          match String.split_on_char ',' pos with
          | [x; y] ->
            (int_of_string x, int_of_string y)
          | _ ->
            failwith ("Invalid position: " ^ pos)
        in
        let parse_line line =
          match String.split ~by:" -> " line with
          | [from; to_] ->
            (parse_pos from, parse_pos to_)
          | _ ->
            failwith ("Invalid line: " ^ line)
        in
        let rec parse_lines acc =
          match read_line in_chan with
          | None ->
            List.rev acc
          | Some line ->
            let line = parse_line line in
            parse_lines (line :: acc)
        in
        parse_lines [] )
  in
  let solve = if !part2 then Day05.Part2.solve else Day05.Part1.solve in
  Printf.printf "%d\n" (solve lines)
