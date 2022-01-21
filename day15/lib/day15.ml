(* Dijkstra's algorithm *)

module Elem = struct
  type t = {key : int; mutable rank : int; mutable idx : int}

  let idx {idx; _} = idx

  let set_idx x idx = x.idx <- idx

  let lt x y = if x.rank = y.rank then x.key < y.key else x.rank < y.rank
end

module IndexedHeap = CCMutHeap.Make (Elem)

let solve riskmap =
  let m = Vector.size riskmap in
  let n = Vector.(size (get riskmap 0)) in
  let nodes =
    Array.init (m * n) (fun x -> Elem.{key = x; rank = Int.max_int; idx = -1})
  in
  let heap = IndexedHeap.create () in
  let source = nodes.(0) in
  source.rank <- 0;
  IndexedHeap.insert heap source;
  let relax u v w =
    if nodes.(u).rank <> Int.max_int && nodes.(u).rank + w < nodes.(v).rank then (
      nodes.(v).rank <- nodes.(u).rank + w;
      if IndexedHeap.in_heap nodes.(v) then IndexedHeap.decrease heap nodes.(v)
      else IndexedHeap.insert heap nodes.(v) )
  in
  while not (IndexedHeap.is_empty heap) do
    let node = IndexedHeap.remove_min heap in
    let u = node.key in
    let x = u / n in
    let y = u mod n in
    List.iter
      (fun (x, y) ->
        if x >= 0 && x < m && y >= 0 && y < n then
          let v = (x * n) + y in
          let w = Vector.(get (get riskmap x) y) in
          relax u v w )
      [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
  done;
  nodes.((m * n) - 1).rank

module Part1 = struct
  let solve = solve
end

module Part2 = struct
  let solve riskmap_orig =
    (* Unfreeze the vector *)
    let riskmap = Vector.(map copy riskmap_orig) in
    let add d x = ((x - 1 + d) mod 9) + 1 in
    (* Extend horizontally *)
    for i = 1 to 4 do
      Vector.iteri
        (fun x row -> Vector.(append row (map (add i) (get riskmap_orig x))))
        riskmap
    done;
    (* Make a copy *)
    let riskmap' = Vector.freeze_copy riskmap in
    (* Extend vertically *)
    for i = 1 to 4 do
      Vector.(append riskmap (map (map (add i)) riskmap'))
    done;
    solve riskmap
end
