let solve timers days =
  let table = Hashtbl.create 9 in
  List.iter
    (fun t ->
      Hashtbl.update table
        ~f:(fun _ -> function None -> Some 1 | Some x -> Some (x + 1))
        ~k:t )
    timers ;
  let rec simulate day =
    if day > days then ()
    else
      let n0 = Hashtbl.get_or table 0 ~default:0 in
      List.iter
        (function
          | 6 ->
            Hashtbl.replace table 6 (n0 + Hashtbl.get_or table 7 ~default:0)
          | 8 ->
            Hashtbl.replace table 8 n0
          | x ->
            Hashtbl.replace table x (Hashtbl.get_or table (x + 1) ~default:0) )
        (List.range 0 8) ;
      simulate (succ day)
  in
  simulate 1 ;
  table |> Hashtbl.to_seq_values |> Seq.fold ( + ) 0
