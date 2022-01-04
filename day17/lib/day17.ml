module Part1 = struct
  let solve ~x:(x1, x2) ~y:(y1, y2) =
    let vertical_steps_to_top d =
      let d = float_of_int d in
      (-1.0 +. sqrt (1.0 +. (d *. 8.0))) /. 2.0 |> floor |> int_of_float
    in
    let rec find max_vy0 vy0 =
      let high = vy0 * (vy0 + 1) / 2 in
      let distance = high - y2 in
      let steps_to_top = vy0 + 1 + vertical_steps_to_top distance in
      let solve_for_vx0 x =
        let k = float_of_int steps_to_top in
        let x = float_of_int x in
        ((2.0 *. x) +. (k *. k) -. k) /. (2.0 *. k) |> floor |> int_of_float
      in
      let validate_vx0 vx0 =
        let k = steps_to_top in
        let x =
          if vx0 - k + 1 > 0 then ((2 * vx0) - k + 1) * k / 2
          else (1 + vx0) * vx0 / 2
        in
        let y = high - ((k - vy0) * (k - vy0 - 1) / 2) in
        let vx = max 0 (vx0 - k) in
        let vy = -(k - vy0) in
        let rec loop x y vx vy =
          if x > x2 || y < y1 then false
          else if x >= x1 && x <= x2 && y >= y1 && y <= y2 then true
          else
            let x = x + vx in
            let y = y + vy in
            let vx = max 0 (vx - 1) in
            let vy = vy - 1 in
            loop x y vx vy
        in
        loop x y vx vy
      in
      if steps_to_top - vy0 > 3 * (y2 - y1) then max_vy0
      else if List.(exists validate_vx0 (range 1 (solve_for_vx0 x2))) then
        find vy0 (succ vy0)
      else find max_vy0 (succ vy0)
    in
    let vy0 = find 0 1 in
    vy0 * (vy0 + 1) / 2
end

module Part2 = struct
  let solve ~x:(x1, x2) ~y:(y1, y2) =
    let rec find acc vy0 =
      let rec steps_to_top k y vy =
        if y + vy < y2 then (k, y, vy)
        else
          let y = y + vy in
          let vy = vy - 1 in
          let k = succ k in
          steps_to_top k y vy
      in
      let k, y, vy = steps_to_top 0 0 vy0 in
      let solve_for_vx0 x =
        if k = 0 then x2
        else
          let k = float_of_int k in
          let x = float_of_int x in
          ((2.0 *. x) +. (k *. k) -. k) /. (2.0 *. k) |> floor |> int_of_float
      in
      let validate_vx0 vx0 =
        let x =
          if vx0 - k + 1 > 0 then ((2 * vx0) - k + 1) * k / 2
          else (1 + vx0) * vx0 / 2
        in
        let vx = max 0 (vx0 - k) in
        let rec loop x y vx vy =
          if x > x2 || y < y1 then false
          else if x >= x1 && x <= x2 && y >= y1 && y <= y2 then true
          else
            let x = x + vx in
            let y = y + vy in
            let vx = max 0 (vx - 1) in
            let vy = vy - 1 in
            loop x y vx vy
        in
        loop x y vx vy
      in
      if k - vy0 > 3 * (y2 - y1) then acc
      else
        let ps =
          List.(
            filter_map
              (fun vx0 -> if validate_vx0 vx0 then Some (vx0, vy0) else None)
              (range 1 (solve_for_vx0 x2)))
        in
        find (acc @ ps) (succ vy0)
    in
    let ps = find [] y1 in
    List.length ps
end
