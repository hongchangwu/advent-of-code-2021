module Frequencies = Map.Make (Int)

let solve frequencies =
  let gamma, epsilon =
    Frequencies.fold
      (fun _ x (gamma, epsilon) ->
        let gamma_delta = if x > 0 then 1 else 0 in
        let epsilon_delta = if x < 0 then 1 else 0 in
        ((gamma lsl 1) + gamma_delta, (epsilon lsl 1) + epsilon_delta) )
      frequencies (0, 0)
  in
  gamma * epsilon
