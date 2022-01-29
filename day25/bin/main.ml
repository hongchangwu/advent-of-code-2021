let usage = "day25 FILE"

let part2 = ref false

let input_file = ref ""

let anon filename = input_file := filename

let () =
  Arg.parse [] anon usage;
  let width, height, east_herd, south_herd =
    IO.(
      let process_line (east_herd, south_herd) i line =
        let east_herd', south_herd' =
          String.foldi
            (fun (east_herd, south_herd) j ch ->
              if Char.equal ch '>' then ((i, j) :: east_herd, south_herd)
              else if Char.equal ch 'v' then (east_herd, (i, j) :: south_herd)
              else (east_herd, south_herd) )
            ([], []) line
        in
        (east_herd @ east_herd', south_herd @ south_herd')
      in
      with_in !input_file (fun in_chan ->
          let lines = read_lines_l in_chan in
          let height = List.length lines in
          let width = String.length (List.hd lines) in
          let east_herd, south_herd = List.foldi process_line ([], []) lines in
          (width, height, east_herd, south_herd) ))
  in
  Printf.printf "%d\n" (Day25.Part1.solve ~width ~height ~east_herd ~south_herd)
