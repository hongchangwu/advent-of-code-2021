let usage = "day20 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let enhancement, light_pixels =
    let parse_enhancement line =
      line |> String.to_seq
      |> Seq.map (function
           | '#' ->
             true
           | '.' ->
             false
           | c ->
             failwith ("Invalid character: " ^ Char.to_string c) )
      |> Vector.of_seq |> Vector.freeze
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let enhancement =
            match read_line in_chan with
            | None ->
              failwith "EOF reached"
            | Some line ->
              parse_enhancement line
          in
          ignore (read_line in_chan) ;
          let light_pixels =
            in_chan |> read_lines_seq
            |> Seq.mapi (fun x line ->
                   line |> String.to_seqi
                   |> Seq.filter_map (fun (y, c) ->
                          if Char.equal c '#' then Some (x, y) else None ) )
            |> Seq.flatten |> List.of_seq
          in
          (enhancement, light_pixels) ))
  in
  let solve = if !part2 then Day20.Part2.solve else Day20.Part1.solve in
  Printf.printf "%d\n" (solve enhancement light_pixels)
