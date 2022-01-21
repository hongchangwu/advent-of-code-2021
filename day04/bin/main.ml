let usage = "day04 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let numbers, boards =
    IO.(
      with_in !input_file (fun in_chan ->
          let parse_numbers line =
            line |> String.split_on_char ',' |> List.map int_of_string
          in
          let numbers =
            in_chan |> read_line
            |> Option.get_exn_or "EOF reached"
            |> parse_numbers
          in
          let parse_board_row line =
            line |> Str.(split (regexp "[ ]+")) |> List.map int_of_string
          in
          let rec parse_boards acc =
            match read_line in_chan with
            | None ->
              List.rev acc
            | Some _ ->
              let rows =
                List.init 5 (fun _ ->
                    in_chan |> read_line
                    |> Option.get_exn_or "EOF reached"
                    |> parse_board_row )
              in
              let board = Day04.Board.make rows in
              parse_boards (board :: acc)
          in
          let boards = parse_boards [] in
          (numbers, boards) ))
  in
  let solve = if !part2 then Day04.Part2.solve else Day04.Part1.solve in
  Printf.printf "%d\n" (solve numbers boards)
