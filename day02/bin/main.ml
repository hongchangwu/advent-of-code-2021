let usage = "day02 FILE"

let aim = ref false

let input_file = ref ""

let specs = [("--aim", Arg.Set aim, "Whether to track the aim value")]

let anon filename = input_file := filename

let parse_line line =
  match String.split_on_char ' ' line with
  | "forward" :: x :: _ ->
    (`Forward, int_of_string x)
  | "up" :: x :: _ ->
    (`Up, int_of_string x)
  | "down" :: x :: _ ->
    (`Down, int_of_string x)
  | _ ->
    failwith ("Invalid line: " ^ line)

let () =
  Arg.parse specs anon usage ;
  let directions =
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          lines |> Seq.map parse_line |> Seq.to_list ))
  in
  Printf.printf "%d\n" (Day02.solve ~aim:!aim directions)
