let usage = "day24 [--part2] FILE"

let part2 = ref false

let input_file = ref ""

let specs = [("--part2", Arg.Set part2, "Solve part 2")]

let anon filename = input_file := filename

let () =
  Arg.parse specs anon usage;
  let instructions =
    let var_of_string : string -> Day24.var = function
      | "w" ->
        W
      | "x" ->
        X
      | "y" ->
        Y
      | "z" ->
        Z
      | s ->
        failwith ("Invalid var: " ^ s)
    in
    let atom_of_string : string -> Day24.atom = function
      | "w" ->
        Var W
      | "x" ->
        Var X
      | "y" ->
        Var Y
      | "z" ->
        Var Z
      | s -> (
        match Int.of_string s with
        | Some x ->
          Val x
        | None ->
          failwith ("Invalid atom: " ^ s) )
    in
    let parse_line line : Day24.instruction =
      match String.split_on_char ' ' line with
      | ["inp"; a] ->
        Inp (var_of_string a)
      | ["add"; a; b] ->
        Add (var_of_string a, atom_of_string b)
      | ["mul"; a; b] ->
        Mul (var_of_string a, atom_of_string b)
      | ["div"; a; b] ->
        Div (var_of_string a, atom_of_string b)
      | ["mod"; a; b] ->
        Mod (var_of_string a, atom_of_string b)
      | ["eql"; a; b] ->
        Eql (var_of_string a, atom_of_string b)
      | _ ->
        failwith ("Invalid line: " ^ line)
    in
    IO.(
      with_in !input_file (fun in_chan ->
          let rec loop acc =
            match read_line in_chan with
            | None ->
              List.rev acc
            | Some line ->
              let x = parse_line line in
              loop (x :: acc)
          in
          loop [] ))
  in
  let solve = if !part2 then Day24.Part2.solve else Day24.Part1.solve in
  Printf.printf "%d\n" (solve instructions)
