module Board = struct
  type t =
    {numbers: (int, int * int) Hashtbl.t; rows: int array; columns: int array}

  let make rows =
    let numbers =
      List.flat_map_i (fun i row -> List.mapi (fun j x -> (x, (i, j))) row) rows
      |> Hashtbl.of_list
    in
    let rows = Array.make 5 0 in
    let columns = Array.make 5 0 in
    {numbers; rows; columns}

  let mark number {numbers; rows; columns} =
    match Hashtbl.get numbers number with
    | None ->
      ()
    | Some (i, j) ->
      rows.(i) <- rows.(i) + 1 ;
      columns.(j) <- columns.(j) + 1 ;
      Hashtbl.remove numbers number

  let has_won {rows; columns; _} =
    Array.exists (( = ) 5) rows || Array.exists (( = ) 5) columns

  let score number ({numbers; _} as board) =
    if has_won board then
      let sum = numbers |> Hashtbl.to_seq_keys |> Seq.fold_left ( + ) 0 in
      Some (number * sum)
    else None
end

let solve numbers boards =
  let rec find_winner number = function
    | [] ->
      None
    | board :: boards ->
      Option.or_lazy
        ~else_:(fun () -> find_winner number boards)
        (Board.score number board)
  in
  let rec aux i = function
    | [] ->
      failwith "No winner"
    | number :: numbers -> (
      List.iter (Board.mark number) boards ;
      if i < 5 then aux (succ i) numbers
      else
        match find_winner number boards with
        | None ->
          aux (succ i) numbers
        | Some score ->
          score )
  in
  aux 0 numbers

let solve_reverse numbers boards =
  let rec aux i boards = function
    | [] ->
      failwith "No winner"
    | number :: numbers -> (
      List.iter (Board.mark number) boards ;
      if i < 5 then aux (succ i) boards numbers
      else
        match
          List.map (fun board -> (board, Board.score number board)) boards
        with
        | [(_, Some score)] ->
          score
        | scores ->
          let boards =
            List.filter_map
              (fun (board, score) ->
                if Option.is_none score then Some board else None )
              scores
          in
          aux (succ i) boards numbers )
  in
  aux 0 boards numbers
