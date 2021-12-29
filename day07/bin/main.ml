let usage = "day07 [--proportional] FILE"

let proportional = ref false

let input_file = ref ""

let specs = [("--proportional", Arg.Set proportional, "Use proportional cost")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let positions =
    IO.(
      with_in !input_file (fun in_chan ->
          read_line in_chan
          |> Option.get_exn_or "EOF reached"
          |> String.split_on_char ',' |> List.map int_of_string ))
  in
  let solve = if !proportional then Day07.solve_proportional else Day07.solve in
  Printf.printf "%d\n" (solve positions)
