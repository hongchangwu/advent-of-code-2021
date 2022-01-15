let mod' x m = ((x - 1) mod m) + 1

(* The deterministic die always rolls 1 first, then 2, then 3, and so on up to 100, after which it starts over at 1 again. *)
module Die = struct
  (* [state] is the next number to be rolled and [out] is the number of times the die has been rolled. *)
  type t = {state : int; out : int}

  let make () = {state = 1; out = 0}

  let out {out; _} = out

  let roll {state; out} =
    let plus x = mod' (state + x) 100 in
    let x = state in
    let y = plus 1 in
    let z = plus 2 in
    let state = plus 3 in
    let out = out + 3 in
    ((x, y, z), {state; out})
end

module Player = struct
  type t = {pos : int; score : int}

  let make pos score = {pos; score}

  let pos {pos; _} = pos

  let score {score; _} = score
end

type turn = Player1 | Player2

let alternate = function Player1 -> Player2 | Player2 -> Player1

let simulate_deterministic (die : Die.t) player1 player2 =
  let rec loop turn die player1 player2 =
    let player, other_player =
      match turn with
      | Player1 ->
        (player1, player2)
      | Player2 ->
        (player2, player1)
    in
    let (x, y, z), die = Die.roll die in
    let pos = mod' (Player.pos player + x + y + z) 10 in
    let score = Player.score player + pos in
    let player = Player.make pos score in
    if score >= 1000 then (turn, Die.out die, player, other_player)
    else
      let player1, player2 =
        match turn with
        | Player1 ->
          (player, other_player)
        | Player2 ->
          (other_player, player)
      in
      let turn = alternate turn in
      loop turn die player1 player2
  in
  loop Player1 die player1 player2

let dirac_distribution = [(3, 1); (4, 3); (5, 6); (6, 7); (7, 6); (8, 3); (9, 1)]

let simulate_dirac player1 player2 =
  let rec loop turn multiplier player1 player2 =
    if Player.score player1 >= 21 then (multiplier, 0)
    else if Player.score player2 >= 21 then (0, multiplier)
    else
      let player, other_player =
        match turn with
        | Player1 ->
          (player1, player2)
        | Player2 ->
          (player2, player1)
      in
      let split value count =
        let pos = mod' (Player.pos player + value) 10 in
        let score = Player.score player + pos in
        let player = Player.make pos score in
        let player1, player2 =
          match turn with
          | Player1 ->
            (player, other_player)
          | Player2 ->
            (other_player, player)
        in
        let turn = alternate turn in
        loop turn (multiplier * count) player1 player2
      in
      List.fold_left
        (fun (p1, p2) (value, count) ->
          let p1', p2' = split value count in
          (p1 + p1', p2 + p2') )
        (0, 0) dirac_distribution
  in
  loop Player1 1 player1 player2

module Part1 = struct
  let solve player1_pos player2_pos =
    let player1 = Player.make player1_pos 0 in
    let player2 = Player.make player2_pos 0 in
    let die = Die.make () in
    let _, out, _, loser = simulate_deterministic die player1 player2 in
    out * Player.score loser
end

module Part2 = struct
  let solve player1_pos player2_pos =
    let player1 = Player.make player1_pos 0 in
    let player2 = Player.make player2_pos 0 in
    let p1, p2 = simulate_dirac player1 player2 in
    max p1 p2
end
