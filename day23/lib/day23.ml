(* ############# *)
(* #12.3.4.5.67# *)
(* ###8#A#C#E### *)
(*   #9#B#D#F#   *)
(*   #########   *)

type amphipod = Amber | Bronze | Copper | Desert

let cost_multiplier = function
  | Amber ->
    1
  | Bronze ->
    10
  | Copper ->
    100
  | Desert ->
    1000

module State = struct
  type t =
    { cell_1 : amphipod option;
      cell_2 : amphipod option;
      cell_3 : amphipod option;
      cell_4 : amphipod option;
      cell_5 : amphipod option;
      cell_6 : amphipod option;
      cell_7 : amphipod option;
      cell_8 : amphipod option;
      cell_9 : amphipod option;
      cell_a : amphipod option;
      cell_b : amphipod option;
      cell_c : amphipod option;
      cell_d : amphipod option;
      cell_e : amphipod option;
      cell_f : amphipod option }

  let make ?cell_1 ?cell_2 ?cell_3 ?cell_4 ?cell_5 ?cell_6 ?cell_7 ?cell_8
      ?cell_9 ?cell_a ?cell_b ?cell_c ?cell_d ?cell_e ?cell_f () =
    { cell_1;
      cell_2;
      cell_3;
      cell_4;
      cell_5;
      cell_6;
      cell_7;
      cell_8;
      cell_9;
      cell_a;
      cell_b;
      cell_c;
      cell_d;
      cell_e;
      cell_f }

  let next_states
      ( { cell_1;
          cell_2;
          cell_3;
          cell_4;
          cell_5;
          cell_6;
          cell_7;
          cell_8;
          cell_9;
          cell_a;
          cell_b;
          cell_c;
          cell_d;
          cell_e;
          cell_f } as state ) =
    let from_cell_1 =
      match cell_1 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_2 && Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_1 = None; cell_8 = Some Amber}, 3)]
          else []
        in
        let to_cell_9 =
          if
            Option.is_none cell_2 && Option.is_none cell_8
            && Option.is_none cell_9
          then [({state with cell_1 = None; cell_9 = Some Amber}, 4)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_1 = None; cell_a = Some Bronze}, 50)]
          else []
        in
        let to_cell_b =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_a && Option.is_none cell_b
          then [({state with cell_1 = None; cell_b = Some Bronze}, 60)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_1 = None; cell_c = Some Copper}, 700)]
          else []
        in
        let to_cell_d =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_c
            && Option.is_none cell_d
          then [({state with cell_1 = None; cell_d = Some Copper}, 800)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_1 = None; cell_e = Some Desert}, 9000)]
          else []
        in
        let to_cell_f =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e && Option.is_none cell_f
          then [({state with cell_1 = None; cell_f = Some Desert}, 10000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_2 =
      match cell_2 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_2 = None; cell_8 = Some Amber}, 2)]
          else []
        in
        let to_cell_9 =
          if Option.is_none cell_8 && Option.is_none cell_9 then
            [({state with cell_2 = None; cell_9 = Some Amber}, 3)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_3 && Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_2 = None; cell_a = Some Bronze}, 40)]
          else []
        in
        let to_cell_b =
          if
            Option.is_none cell_3 && Option.is_none cell_a
            && Option.is_none cell_b
          then [({state with cell_2 = None; cell_b = Some Bronze}, 50)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_2 = None; cell_c = Some Copper}, 600)]
          else []
        in
        let to_cell_d =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_c && Option.is_none cell_d
          then [({state with cell_2 = None; cell_d = Some Copper}, 700)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_2 = None; cell_e = Some Desert}, 8000)]
          else []
        in
        let to_cell_f =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_e
            && Option.is_none cell_f
          then [({state with cell_2 = None; cell_f = Some Desert}, 9000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_3 =
      match cell_3 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_3 = None; cell_8 = Some Amber}, 2)]
          else []
        in
        let to_cell_9 =
          if Option.is_none cell_8 && Option.is_none cell_9 then
            [({state with cell_3 = None; cell_9 = Some Amber}, 3)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_3 = None; cell_a = Some Bronze}, 20)]
          else []
        in
        let to_cell_b =
          if Option.is_none cell_a && Option.is_none cell_b then
            [({state with cell_3 = None; cell_b = Some Bronze}, 30)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_4 && Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_3 = None; cell_c = Some Copper}, 400)]
          else []
        in
        let to_cell_d =
          if
            Option.is_none cell_4 && Option.is_none cell_c
            && Option.is_none cell_d
          then [({state with cell_3 = None; cell_d = Some Copper}, 500)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_3 = None; cell_e = Some Desert}, 6000)]
          else []
        in
        let to_cell_f =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e && Option.is_none cell_f
          then [({state with cell_3 = None; cell_f = Some Desert}, 7000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_4 =
      match cell_4 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_3 && Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_4 = None; cell_8 = Some Amber}, 4)]
          else []
        in
        let to_cell_9 =
          if
            Option.is_none cell_3 && Option.is_none cell_8
            && Option.is_none cell_9
          then [({state with cell_4 = None; cell_9 = Some Amber}, 5)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_4 = None; cell_a = Some Bronze}, 20)]
          else []
        in
        let to_cell_b =
          if Option.is_none cell_a && Option.is_none cell_b then
            [({state with cell_4 = None; cell_b = Some Bronze}, 30)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_4 = None; cell_c = Some Copper}, 200)]
          else []
        in
        let to_cell_d =
          if Option.is_none cell_c && Option.is_none cell_d then
            [({state with cell_4 = None; cell_d = Some Copper}, 300)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_5 && Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_4 = None; cell_e = Some Desert}, 4000)]
          else []
        in
        let to_cell_f =
          if
            Option.is_none cell_5 && Option.is_none cell_e
            && Option.is_none cell_f
          then [({state with cell_4 = None; cell_f = Some Desert}, 5000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_5 =
      match cell_5 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_5 = None; cell_8 = Some Amber}, 6)]
          else []
        in
        let to_cell_9 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_8 && Option.is_none cell_9
          then [({state with cell_5 = None; cell_9 = Some Amber}, 7)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_4 && Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_5 = None; cell_a = Some Bronze}, 40)]
          else []
        in
        let to_cell_b =
          if
            Option.is_none cell_4 && Option.is_none cell_a
            && Option.is_none cell_b
          then [({state with cell_5 = None; cell_b = Some Bronze}, 50)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_5 = None; cell_c = Some Copper}, 200)]
          else []
        in
        let to_cell_d =
          if Option.is_none cell_c && Option.is_none cell_d then
            [({state with cell_5 = None; cell_d = Some Copper}, 300)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_5 = None; cell_e = Some Desert}, 2000)]
          else []
        in
        let to_cell_f =
          if Option.is_none cell_e && Option.is_none cell_f then
            [({state with cell_5 = None; cell_f = Some Desert}, 3000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_6 =
      match cell_6 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_6 = None; cell_8 = Some Amber}, 8)]
          else []
        in
        let to_cell_9 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_8
            && Option.is_none cell_9
          then [({state with cell_6 = None; cell_9 = Some Amber}, 9)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_6 = None; cell_a = Some Bronze}, 60)]
          else []
        in
        let to_cell_b =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_a && Option.is_none cell_b
          then [({state with cell_6 = None; cell_b = Some Bronze}, 70)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_5 && Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_6 = None; cell_c = Some Copper}, 400)]
          else []
        in
        let to_cell_d =
          if
            Option.is_none cell_5 && Option.is_none cell_c
            && Option.is_none cell_d
          then [({state with cell_6 = None; cell_d = Some Copper}, 500)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_6 = None; cell_e = Some Desert}, 2000)]
          else []
        in
        let to_cell_f =
          if Option.is_none cell_e && Option.is_none cell_f then
            [({state with cell_6 = None; cell_f = Some Desert}, 3000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_7 =
      match cell_7 with
      | None ->
        []
      | Some Amber ->
        let to_cell_8 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_8
            && match cell_9 with Some Amber -> true | _ -> false
          then [({state with cell_7 = None; cell_8 = Some Amber}, 9)]
          else []
        in
        let to_cell_9 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_8 && Option.is_none cell_9
          then [({state with cell_7 = None; cell_9 = Some Amber}, 10)]
          else []
        in
        to_cell_8 @ to_cell_9
      | Some Bronze ->
        let to_cell_a =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6 && Option.is_none cell_a
            && match cell_b with Some Bronze -> true | _ -> false
          then [({state with cell_7 = None; cell_a = Some Bronze}, 70)]
          else []
        in
        let to_cell_b =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6 && Option.is_none cell_a
            && Option.is_none cell_b
          then [({state with cell_7 = None; cell_b = Some Bronze}, 80)]
          else []
        in
        to_cell_a @ to_cell_b
      | Some Copper ->
        let to_cell_c =
          if
            Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_c
            && match cell_d with Some Copper -> true | _ -> false
          then [({state with cell_7 = None; cell_c = Some Copper}, 500)]
          else []
        in
        let to_cell_d =
          if
            Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_c && Option.is_none cell_d
          then [({state with cell_7 = None; cell_d = Some Copper}, 600)]
          else []
        in
        to_cell_c @ to_cell_d
      | Some Desert ->
        let to_cell_e =
          if
            Option.is_none cell_6 && Option.is_none cell_e
            && match cell_f with Some Desert -> true | _ -> false
          then [({state with cell_7 = None; cell_e = Some Desert}, 3000)]
          else []
        in
        let to_cell_f =
          if
            Option.is_none cell_6 && Option.is_none cell_e
            && Option.is_none cell_f
          then [({state with cell_7 = None; cell_f = Some Desert}, 4000)]
          else []
        in
        to_cell_e @ to_cell_f
    in
    let from_cell_8 =
      match cell_8 with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if Option.is_none cell_1 && Option.is_none cell_2 then
            [ ( {state with cell_8 = None; cell_1 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if Option.is_none cell_2 then
            [ ( {state with cell_8 = None; cell_2 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if Option.is_none cell_3 then
            [ ( {state with cell_8 = None; cell_3 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_3 && Option.is_none cell_4 then
            [ ( {state with cell_8 = None; cell_4 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5
          then
            [ ( {state with cell_8 = None; cell_5 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
          then
            [ ( {state with cell_8 = None; cell_6 = Some x},
                8 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_7
          then
            [ ( {state with cell_8 = None; cell_7 = Some x},
                9 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_9 =
      match cell_9 with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_8
          then
            [ ( {state with cell_9 = None; cell_1 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if Option.is_none cell_2 && Option.is_none cell_8 then
            [ ( {state with cell_9 = None; cell_2 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if Option.is_none cell_3 && Option.is_none cell_8 then
            [ ( {state with cell_9 = None; cell_3 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_8
          then
            [ ( {state with cell_9 = None; cell_4 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_8
          then
            [ ( {state with cell_9 = None; cell_5 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_8
          then
            [ ( {state with cell_9 = None; cell_6 = Some x},
                9 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_7 && Option.is_none cell_8
          then
            [ ( {state with cell_9 = None; cell_7 = Some x},
                10 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_a =
      match cell_a with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3
          then
            [ ( {state with cell_a = None; cell_1 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if Option.is_none cell_2 && Option.is_none cell_3 then
            [ ( {state with cell_a = None; cell_2 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if Option.is_none cell_3 then
            [ ( {state with cell_a = None; cell_3 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_4 then
            [ ( {state with cell_a = None; cell_4 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if Option.is_none cell_4 && Option.is_none cell_5 then
            [ ( {state with cell_a = None; cell_5 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6
          then
            [ ( {state with cell_a = None; cell_6 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6 && Option.is_none cell_7
          then
            [ ( {state with cell_a = None; cell_7 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_b =
      match cell_b with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3 && Option.is_none cell_a
          then
            [ ( {state with cell_b = None; cell_1 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_a
          then
            [ ( {state with cell_b = None; cell_2 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if Option.is_none cell_3 && Option.is_none cell_a then
            [ ( {state with cell_b = None; cell_3 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_4 && Option.is_none cell_a then
            [ ( {state with cell_b = None; cell_4 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_a
          then
            [ ( {state with cell_b = None; cell_5 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6 && Option.is_none cell_a
          then
            [ ( {state with cell_b = None; cell_6 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_6 && Option.is_none cell_7
            && Option.is_none cell_a
          then
            [ ( {state with cell_b = None; cell_7 = Some x},
                8 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_c =
      match cell_c with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3 && Option.is_none cell_4
          then
            [ ( {state with cell_c = None; cell_1 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4
          then
            [ ( {state with cell_c = None; cell_2 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if Option.is_none cell_3 && Option.is_none cell_4 then
            [ ( {state with cell_c = None; cell_3 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_4 then
            [ ( {state with cell_c = None; cell_4 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if Option.is_none cell_5 then
            [ ( {state with cell_c = None; cell_5 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if Option.is_none cell_5 && Option.is_none cell_6 then
            [ ( {state with cell_c = None; cell_6 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_7
          then
            [ ( {state with cell_c = None; cell_7 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_d =
      match cell_d with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_c
          then
            [ ( {state with cell_d = None; cell_1 = Some x},
                8 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_c
          then
            [ ( {state with cell_d = None; cell_2 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_c
          then
            [ ( {state with cell_d = None; cell_3 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_4 && Option.is_none cell_c then
            [ ( {state with cell_d = None; cell_4 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if Option.is_none cell_5 && Option.is_none cell_c then
            [ ( {state with cell_d = None; cell_5 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if
            Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_c
          then
            [ ( {state with cell_d = None; cell_6 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_5 && Option.is_none cell_6
            && Option.is_none cell_7 && Option.is_none cell_c
          then
            [ ( {state with cell_d = None; cell_7 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_e =
      match cell_e with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5
          then
            [ ( {state with cell_e = None; cell_1 = Some x},
                9 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_5
          then
            [ ( {state with cell_e = None; cell_2 = Some x},
                8 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5
          then
            [ ( {state with cell_e = None; cell_3 = Some x},
                6 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if Option.is_none cell_4 && Option.is_none cell_5 then
            [ ( {state with cell_e = None; cell_4 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if Option.is_none cell_5 then
            [ ( {state with cell_e = None; cell_5 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if Option.is_none cell_6 then
            [ ( {state with cell_e = None; cell_6 = Some x},
                2 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if Option.is_none cell_6 && Option.is_none cell_7 then
            [ ( {state with cell_e = None; cell_7 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    let from_cell_f =
      match cell_f with
      | None ->
        []
      | Some x ->
        let to_cell_1 =
          if
            Option.is_none cell_1 && Option.is_none cell_2
            && Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_e
          then
            [ ( {state with cell_f = None; cell_1 = Some x},
                10 * cost_multiplier x ) ]
          else []
        in
        let to_cell_2 =
          if
            Option.is_none cell_2 && Option.is_none cell_3
            && Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e
          then
            [ ( {state with cell_f = None; cell_2 = Some x},
                9 * cost_multiplier x ) ]
          else []
        in
        let to_cell_3 =
          if
            Option.is_none cell_3 && Option.is_none cell_4
            && Option.is_none cell_5 && Option.is_none cell_e
          then
            [ ( {state with cell_f = None; cell_3 = Some x},
                7 * cost_multiplier x ) ]
          else []
        in
        let to_cell_4 =
          if
            Option.is_none cell_4 && Option.is_none cell_5
            && Option.is_none cell_e
          then
            [ ( {state with cell_f = None; cell_4 = Some x},
                5 * cost_multiplier x ) ]
          else []
        in
        let to_cell_5 =
          if Option.is_none cell_5 && Option.is_none cell_e then
            [ ( {state with cell_f = None; cell_5 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_6 =
          if Option.is_none cell_6 && Option.is_none cell_e then
            [ ( {state with cell_f = None; cell_6 = Some x},
                3 * cost_multiplier x ) ]
          else []
        in
        let to_cell_7 =
          if
            Option.is_none cell_6 && Option.is_none cell_7
            && Option.is_none cell_e
          then
            [ ( {state with cell_f = None; cell_7 = Some x},
                4 * cost_multiplier x ) ]
          else []
        in
        to_cell_1 @ to_cell_2 @ to_cell_3 @ to_cell_4 @ to_cell_5 @ to_cell_6
        @ to_cell_7
    in
    from_cell_1 @ from_cell_2 @ from_cell_3 @ from_cell_4 @ from_cell_5
    @ from_cell_6 @ from_cell_7 @ from_cell_8 @ from_cell_9 @ from_cell_a
    @ from_cell_b @ from_cell_c @ from_cell_d @ from_cell_e @ from_cell_f
end

module Elem = struct
  type t = {key : State.t; mutable rank : int; mutable idx : int}

  let idx {idx; _} = idx

  let set_idx x idx = x.idx <- idx

  let lt x y = x.rank < y.rank
end

module IndexedHeap = CCMutHeap.Make (Elem)

module Part1 = struct
  let solve
      ((cell_8, cell_9), (cell_a, cell_b), (cell_c, cell_d), (cell_e, cell_f)) =
    let init_state =
      State.make ~cell_8 ~cell_9 ~cell_a ~cell_b ~cell_c ~cell_d ~cell_e ~cell_f
        ()
    in
    let target_state =
      State.make ~cell_8:Amber ~cell_9:Amber ~cell_a:Bronze ~cell_b:Bronze
        ~cell_c:Copper ~cell_d:Copper ~cell_e:Desert ~cell_f:Desert ()
    in
    let source = Elem.{key = init_state; rank = 0; idx = -1} in
    let cost_map = Hashtbl.create 100000 in
    Hashtbl.add cost_map init_state source;
    let heap = IndexedHeap.create () in
    IndexedHeap.insert heap source;
    let relax u v w =
      match Hashtbl.get cost_map u with
      | Some x
        when x.rank + w
             < ( Hashtbl.get cost_map v
               |> Option.fold (fun _ ({rank; _} : Elem.t) -> rank) Int.max_int
               ) ->
        let y =
          Hashtbl.get_or_add cost_map
            ~f:(Fun.const Elem.{key = v; rank = 0; idx = -1})
            ~k:v
        in
        y.rank <- x.rank + w;
        if IndexedHeap.in_heap y then IndexedHeap.decrease heap y
        else IndexedHeap.insert heap y
      | _ ->
        ()
    in
    while not (IndexedHeap.is_empty heap) do
      let Elem.{key = u; _} = IndexedHeap.remove_min heap in
      List.iter (fun (v, w) -> relax u v w) (State.next_states u)
    done;
    (Option.get_exn_or "Unreachable" (Hashtbl.get cost_map target_state)).rank
end

module Part2 = struct
  let solve _ = failwith "FIXME"
end
