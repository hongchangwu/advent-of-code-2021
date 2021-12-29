let sum = List.fold_left ( + ) 0

let solve positions =
  let sorted_positions = List.sort Int.compare positions in
  (* This not a true median, but since we will only use it as a starting point, it will suffice. *)
  let median = List.nth sorted_positions (List.length positions / 2) in
  let low, remaining =
    List.take_drop_while (fun x -> x < median) sorted_positions
  in
  let middle, high = List.take_drop_while (fun x -> x = median) remaining in
  let sum_low, num_low =
    List.fold_left (fun (sum, num) x -> (sum + x, num + 1)) (0, 0) low
  in
  let cost_low = (num_low * median) - sum_low in
  let sum_high, num_high =
    List.fold_left (fun (sum, num) x -> (sum + x, num + 1)) (0, 0) high
  in
  let cost_high = sum_high - (num_high * median) in
  let num_middle = List.length middle in
  let rec minimize cost num1 num2 pivot next = function
    | [] ->
      cost
    | x :: _ as xs ->
      let cost' = cost + num1 - num2 in
      if cost' > cost then cost
      else if x = pivot then
        let us, vs = List.take_drop_while (( = ) pivot) xs in
        let num1' = num1 + List.length us in
        let num2' = num2 - List.length us in
        minimize cost' num1' num2' (next pivot) next vs
      else minimize cost' num1 num2 (next pivot) next xs
  in
  if cost_high > cost_low then
    minimize (cost_low + cost_high) (num_low + num_middle) num_high
      (succ median) succ high
  else if cost_high > cost_low then
    minimize (cost_low + cost_high) (num_high + num_middle) num_low
      (pred median) pred (List.rev low)
  else cost_low + cost_high

let solve_proportional positions =
  let sorted_positions = List.sort Int.compare positions in
  (* This not a true median, but since we will only use it as a starting point, it will suffice. *)
  let median = List.nth sorted_positions (List.length positions / 2) in
  let low, remaining =
    List.take_drop_while (fun x -> x < median) sorted_positions
  in
  let middle, high = List.take_drop_while (fun x -> x = median) remaining in
  let diffs_low = List.map (fun x -> median - x) low in
  let cost_low =
    List.fold_left (fun acc d -> acc + (d * (d + 1) / 2)) 0 diffs_low
  in
  let diffs_high = List.map (fun x -> x - median) high in
  let cost_high =
    List.fold_left (fun acc d -> acc + (d * (d + 1) / 2)) 0 diffs_high
  in
  let diffs_middle = List.init (List.length middle) (Fun.const 0) in
  let rec minimize cost diffs1 diffs2 pivot next = function
    | [] ->
      cost
    | x :: _ as xs ->
      let cost' = cost + sum diffs1 - sum diffs2 in
      if cost' > cost then cost
      else if x = pivot then
        let us, vs = List.take_drop_while (( = ) pivot) xs in
        let k = List.length us in
        let diffs1' = List.map succ diffs1 @ List.init k (Fun.const 1) in
        let diffs2' = List.map pred (List.drop k diffs2) in
        minimize cost' diffs1' diffs2' (next pivot) next vs
      else
        let diffs1' = List.map succ diffs1 in
        let diffs2' = List.map pred diffs2 in
        minimize cost' diffs1' diffs2' (next pivot) next xs
  in
  if cost_high > cost_low then
    minimize (cost_low + cost_high)
      (List.map succ (diffs_low @ diffs_middle))
      diffs_high (succ median) succ high
  else if cost_high > cost_low then
    minimize (cost_low + cost_high)
      (List.map succ (diffs_middle @ diffs_high))
      (List.rev diffs_low) (pred median) pred (List.rev low)
  else cost_low + cost_high
