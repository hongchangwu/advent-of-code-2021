let usage = "day03-2 FILE"

let input_file = ref ""

let anon filename = input_file := filename

let () =
  Arg.parse [] anon usage ;
  let readings = IO.(with_in !input_file read_lines_l) in
  Printf.printf "%d\n" (Day03.Part2.solve readings)
