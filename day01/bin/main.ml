let usage = "day01 FILE"

let window_size = ref 1

let input_file = ref ""

let specs =
  [("--window", Arg.Set_int window_size, "The length of the sliding window")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage ;
  let numbers =
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          lines |> Seq.map int_of_string |> Seq.to_list ))
  in
  Printf.printf "%d\n" (Day01.solve ~window:!window_size numbers)
