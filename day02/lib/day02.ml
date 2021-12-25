type direction = [`Forward | `Down | `Up]

let solve ~aim directions =
  let horizontal, depth =
    if aim then
      let advance (horizontal, depth, aim) (direction, value) =
        match direction with
        | `Forward ->
          (horizontal + value, depth + (aim * value), aim)
        | `Down ->
          (horizontal, depth, aim + value)
        | `Up ->
          (horizontal, depth, aim - value)
      in
      let horizontal, depth, _ = List.fold_left advance (0, 0, 0) directions in
      (horizontal, depth)
    else
      let advance (horizontal, depth) (direction, value) =
        match direction with
        | `Forward ->
          (horizontal + value, depth)
        | `Down ->
          (horizontal, depth + value)
        | `Up ->
          (horizontal, depth - value)
      in
      List.fold_left advance (0, 0) directions
  in
  horizontal * depth
