open Fun.Infix

let usage = "day03-1 FILE"

let input_file = ref ""

let anon filename = input_file := filename

let () =
  Arg.parse [] anon usage ;
  let frequencies =
    let table = Hashtbl.create 32 in
    let f i = function
      | '0' ->
        Hashtbl.update table ~k:i
          ~f:(Fun.const @@ (Option.fold ( + ) (-1) %> Option.some))
      | '1' ->
        Hashtbl.update table ~k:i
          ~f:(Fun.const @@ (Option.fold ( + ) 1 %> Option.some))
      | c ->
        failwith ("Invalid digit: " ^ Char.to_string c)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let lines = read_lines_seq in_chan in
          Seq.iter (String.iteri f) lines )) ;
    table |> Hashtbl.to_seq |> Day03.Part1.Frequencies.of_seq
  in
  Printf.printf "%d\n" (Day03.Part1.solve frequencies)
