let usage = "day14 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let template, rules =
    IO.(
      with_in !input_file (fun in_chan ->
          let parse_template () =
            match read_line in_chan with
            | None ->
              failwith "EOF reached"
            | Some line ->
              ignore (read_line in_chan) ;
              line
          in
          let parse_rule line =
            match String.split ~by:" -> " line with
            | [from; to_] ->
              ((String.get from 0, String.get from 1), String.get to_ 0)
            | _ ->
              failwith ("Invalid line: " ^ line)
          in
          let rec parse_rules acc =
            match read_line in_chan with
            | None ->
              List.rev acc
            | Some line ->
              let rule = parse_rule line in
              parse_rules (rule :: acc)
          in
          let template = parse_template () in
          let rules = parse_rules [] in
          (template, rules) ))
  in
  let solve = if !part2 then Day14.Part2.solve else Day14.Part1.solve in
  Printf.printf "%d\n" (solve template rules)
