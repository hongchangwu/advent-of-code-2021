let usage = "day22 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let instructions =
    let action_of_string : string -> Day22.action = function
      | "on" ->
        On
      | "off" ->
        Off
      | _ ->
        failwith "action_of_string"
    in
    let parse_line line : Day22.instruction =
      let re =
        Str.regexp
          {|^\([a-z]+\)[ ]x=\(-?[0-9]+\)[.][.]\(-?[0-9]+\),y=\(-?[0-9]+\)[.][.]\(-?[0-9]+\),z=\(-?[0-9]+\)[.][.]\(-?[0-9]+\)$|}
      in
      if Str.string_match re line 0 then
        let action = line |> Str.matched_group 1 |> action_of_string in
        let x1 = line |> Str.matched_group 2 |> int_of_string in
        let x2 = line |> Str.matched_group 3 |> int_of_string in
        let y1 = line |> Str.matched_group 4 |> int_of_string in
        let y2 = line |> Str.matched_group 5 |> int_of_string in
        let z1 = line |> Str.matched_group 6 |> int_of_string in
        let z2 = line |> Str.matched_group 7 |> int_of_string in
        {action; cuboid = {x1; x2; y1; y2; z1; z2}}
      else failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          lines |> Seq.map parse_line |> Seq.to_list ))
  in
  let solve = if !part2 then Day22.Part2.solve else Day22.Part1.solve in
  Printf.printf "%d\n" (solve instructions)
