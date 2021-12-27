let usage = "day05 FILE"

let diagonal = ref false

let input_file = ref ""

let specs = [("--diagonal", Arg.Set diagonal, "Include diagonal lines")]

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
  Printf.printf "%d\n" (Day05.solve ~diagonal:!diagonal lines)
