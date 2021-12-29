let solve timers days =
  let table = Hashtbl.create 9 in
  List.iter
    (fun k ->
      Hashtbl.update table
        ~f:(fun _ -> function None -> Some 1 | Some x -> Some (x + 1))
        ~k )
    timers ;
  let rec simulate day =
    if day > days then ()
    else
      let n0 = Hashtbl.get_or table 0 ~default:0 in
      List.iter
        (fun k ->
          let v =
            match k with
            | 6 ->
              n0 + Hashtbl.get_or table 7 ~default:0
            | 8 ->
              n0
            | x ->
              Hashtbl.get_or table (x + 1) ~default:0
          in
          Hashtbl.replace table k v )
        (List.range 0 8) ;
      simulate (succ day)
  in
  simulate 1 ;
  table |> Hashtbl.to_seq_values |> Seq.fold ( + ) 0
