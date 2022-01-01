module StringSet = Set.Make (String)

module Part1 = struct
  let solve edges =
    let graph = Hashtbl.create 10 in
    List.iter
      (fun (a, b) ->
        Hashtbl.update graph ~k:a ~f:(fun _ -> function
          | None -> Some [b] | Some vs -> Some (b :: vs) ) ;
        Hashtbl.update graph ~k:b ~f:(fun _ -> function
          | None -> Some [a] | Some vs -> Some (a :: vs) ) )
      edges ;
    let rec dfs paths path visited node =
      if String.equal node "end" then List.rev ("end" :: path) :: paths
      else if StringSet.mem node visited then paths
      else
        let visited' =
          if
            String.equal node "start"
            || String.(equal node (lowercase_ascii node))
          then StringSet.add node visited
          else visited
        in
        let path' = node :: path in
        List.fold_left
          (fun paths node -> dfs paths path' visited' node)
          paths
          (Hashtbl.get_or graph node ~default:[])
    in
    dfs [] [] StringSet.empty "start" |> List.length
end

module Part2 = struct
  module Path = struct
    type t = string list

    let compare = List.compare String.compare
  end

  module PathSet = Set.Make (Path)

  let solve edges =
    let graph = Hashtbl.create 10 in
    List.iter
      (fun (a, b) ->
        Hashtbl.update graph ~k:a ~f:(fun _ -> function
          | None -> Some [b] | Some vs -> Some (b :: vs) ) ;
        Hashtbl.update graph ~k:b ~f:(fun _ -> function
          | None -> Some [a] | Some vs -> Some (a :: vs) ) )
      edges ;
    let rec dfs paths path chosen visited node =
      if String.equal node "end" then
        PathSet.add (List.rev ("end" :: path)) paths
      else if StringSet.mem node visited then paths
      else
        let visited', chosen', alternative =
          let visited' = StringSet.add node visited in
          if String.equal node "start" then (visited', chosen, None)
          else if String.(equal node (lowercase_ascii node)) then
            match chosen with
            | `None ->
              (visited', chosen, Some (`Once node, visited))
            | `Once n when String.equal n node ->
              (visited', `Twice, None)
            | _ ->
              (visited', chosen, None)
          else (visited, chosen, None)
        in
        let path' = node :: path in
        List.fold_left
          (fun paths node ->
            let paths = dfs paths path' chosen' visited' node in
            match alternative with
            | None ->
              paths
            | Some (chosen'', visited'') ->
              dfs paths path' chosen'' visited'' node )
          paths
          (Hashtbl.get_or graph node ~default:[])
    in
    dfs PathSet.empty [] `None StringSet.empty "start" |> PathSet.cardinal
end
