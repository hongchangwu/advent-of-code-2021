open Fun.Infix

module Tree = struct
  type t = {left: elem; right: elem}

  and elem = Atom of int | Tree of t

  let atom x = Atom x

  let tree t = Tree t

  let make left right = {left; right}

  let parse =
    let open Angstrom in
    let int =
      int_of_string <$> take_while1 (function '0' .. '9' -> true | _ -> false)
    in
    let tree =
      fix (fun t ->
          let elem = atom <$> int <|> (tree <$> t) in
          Fun.uncurry make
          <$> char '[' *> both (elem <* char ',') elem
          <* char ']' )
    in
    parse_string ~consume:Consume.All tree %> Result.get_or_failwith

  let rec pp_elem fmt = function
    | Atom x ->
      Format.int fmt x
    | Tree t ->
      pp fmt t

  and pp fmt {left; right} =
    Format.fprintf fmt "[%a,%a]" pp_elem left pp_elem right

  module Zipper = struct
    type direction = Left of elem | Right of elem

    type path = direction list

    type t = elem * path

    let make t : t = (Tree t, [])

    let up : t -> t option =
     fun (elem, path) ->
      match path with
      | [] ->
        None
      | Left right :: path ->
        Some (Tree {left= elem; right}, path)
      | Right left :: path ->
        Some (Tree {left; right= elem}, path)

    let rec root : t -> t =
     fun loc -> match up loc with None -> loc | Some loc -> root loc

    let rec modify_leftmost f = function
      | Atom x ->
        Atom (f x)
      | Tree ({left; _} as tree) ->
        let left = modify_leftmost f left in
        Tree {tree with left}

    let rec modify_rightmost f = function
      | Atom x ->
        Atom (f x)
      | Tree ({right; _} as tree) ->
        let right = modify_rightmost f right in
        Tree {tree with right}

    let rec modify_left f = function
      | [] ->
        []
      | Right l :: path ->
        Right (modify_rightmost f l) :: path
      | Left r :: path ->
        let path = modify_left f path in
        Left r :: path

    let rec modify_right f = function
      | [] ->
        []
      | Left r :: path ->
        Left (modify_leftmost f r) :: path
      | Right l :: path ->
        let path = modify_right f path in
        Right l :: path
  end

  type action = Explode of Zipper.t | Split of Zipper.t

  let get_loc = function Explode loc | Split loc -> loc

  let reduce t =
    let loc = Zipper.make t in
    let rec reduce_once (elem, path) level acc =
      match elem with
      (* explode *)
      | Tree {left= Atom a; right= Atom b} when level >= 4 ->
        let path =
          Zipper.(path |> modify_left (( + ) a) |> modify_right (( + ) b))
        in
        Explode (Atom 0, path) :: acc
      (* split *)
      | Atom x when x >= 10 ->
        let a = x / 2 in
        let b = x - a in
        let tree = Tree {left= Atom a; right= Atom b} in
        Split (tree, path) :: acc
      | Atom _ ->
        acc
      | Tree {left; right} ->
        let level = succ level in
        acc
        |> reduce_once (left, Zipper.Left right :: path) level
        |> reduce_once (right, Zipper.Right left :: path) level
    in
    let rec reduce' loc =
      match List.rev (reduce_once loc 0 []) with
      | [] ->
        loc
      | action :: _ as actions ->
        let loc =
          match
            List.find_opt (function Explode _ -> true | _ -> false) actions
          with
          | Some action ->
            get_loc action
          | None ->
            get_loc action
        in
        loc |> Zipper.root |> reduce'
    in
    match fst (reduce' loc) with
    | Atom _ ->
      failwith "Atom"
    | Tree {left; right} ->
      make left right

  let magnitude {left; right} =
    let rec mag = function
      | Atom x ->
        x
      | Tree {left; right} ->
        (3 * mag left) + (2 * mag right)
    in
    (3 * mag left) + (2 * mag right)
end

module Part1 = struct
  let solve = function
    | [] ->
      0
    | [tree] ->
      Tree.magnitude tree
    | tree :: trees ->
      List.fold_left
        (fun tree1 tree2 -> Tree.(reduce (make (tree tree1) (tree tree2))))
        tree trees
      |> Tree.magnitude
end

module Part2 = struct
  let solve = function
    | [] ->
      0
    | [tree] ->
      Tree.magnitude tree
    | trees ->
      let trees = Array.of_list trees in
      let n = Array.length trees in
      let max_magnitude = ref 0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if i <> j then
            let tree1 = trees.(i) in
            let tree2 = trees.(j) in
            let magnitude =
              Tree.(magnitude (reduce (make (tree tree1) (tree tree2))))
            in
            max_magnitude := max magnitude !max_magnitude
        done
      done ;
      !max_magnitude
end
