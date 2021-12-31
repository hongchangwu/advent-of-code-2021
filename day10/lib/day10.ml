open Fun.Infix

let is_open = function '(' | '[' | '{' | '<' -> true | _ -> false

let is_close = function ')' | ']' | '}' | '>' -> true | _ -> false

let is_matching open_ close =
  match (open_, close) with
  | '(', ')' | '[', ']' | '{', '}' | '<', '>' ->
    true
  | _ ->
    false

module Part1 = struct
  let solve =
    let calc_point = function
      | ')' ->
        3
      | ']' ->
        57
      | '}' ->
        1197
      | '>' ->
        25137
      | _ ->
        0
    in
    let rec solve_one stack = function
      | [] ->
        0
      | c :: cs ->
        if is_open c then solve_one (c :: stack) cs
        else if is_close c then
          match stack with
          | [] ->
            calc_point c
          | c' :: stack' ->
            if is_matching c' c then solve_one stack' cs else calc_point c
        else failwith ("Invalid character: " ^ Char.to_string c)
    in
    List.fold_left (fun acc line -> acc + solve_one [] (String.to_list line)) 0
end

module Part2 = struct
  let solve lines =
    let calc_point = function
      | '(' ->
        1
      | '[' ->
        2
      | '{' ->
        3
      | '<' ->
        4
      | _ ->
        0
    in
    let calc_points =
      List.fold_left (fun acc c -> (acc * 5) + calc_point c) 0
    in
    let rec solve_one stack = function
      | [] -> (
        match stack with [] -> None | _ -> Some (calc_points stack) )
      | c :: cs ->
        if is_open c then solve_one (c :: stack) cs
        else if is_close c then
          match stack with
          | [] ->
            None
          | c' :: stack' ->
            if is_matching c' c then solve_one stack' cs else None
        else failwith ("Invalid character: " ^ Char.to_string c)
    in
    let points =
      List.filter_map (String.to_list %> solve_one []) lines
      |> List.sort Int.compare
    in
    List.nth points (List.length points / 2)
end
