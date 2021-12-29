let usage = "day06 FILE DAYS"

let input_file = ref ""

let days = ref 0

let specs =
  [("--days", Arg.Set_int days, "The number of days for the simulation")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let timers =
    IO.(
      with_in !input_file (fun in_chan ->
          read_line in_chan
          |> Option.get_exn_or "EOF reached"
          |> String.split_on_char ',' |> List.map int_of_string ))
  in
  Printf.printf "%d\n" (Day06.solve timers !days)
