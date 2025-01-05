  module type OrderedType = sig
    type t
    val compare : t -> t -> int
  end

  module Node = struct
    type t = { x: float; y: float; flag: bool }

    let compare a b =
      match Float.compare a.x b.x with
      | 0 -> (match Float.compare a.y b.y with
              | 0 -> Bool.compare a.flag b.flag
              | cmp -> cmp)
      | cmp -> cmp
  end

  module type Graph =
  sig
      type node

      module NodeSet : Set.S with type elt = node;;

      type graph

      val empty : graph

      val is_empty : graph -> bool

      val add_node : node -> graph -> graph

      val add_edge : node -> node -> graph -> graph

      val succs : node -> graph -> NodeSet.t

      val fold : (node -> 'a -> 'a) -> graph -> 'a -> 'a

      val remove_edge : node -> node -> graph -> graph

      val remove_node : node -> graph -> graph
  end

  module Make(N:Set.OrderedType) : Graph with type node = N.t = struct
    type node = N.t

    module NodeSet = Set.Make(N)

    module NodeMap = Map.Make(N)

    type graph = NodeSet.t NodeMap.t

    let empty = NodeMap.empty

    let is_empty m = NodeMap.is_empty m

    let add_node n m =
        try
            let _ = NodeMap.find n m in
            m
        with
        | Not_found -> NodeMap.add n NodeSet.empty m

    let add_edge n n' g =
      let g = add_node n (add_node n' g) in
      let g = NodeMap.add n (NodeSet.add n' (NodeMap.find n g)) g in
      NodeMap.add n' (NodeSet.add n (NodeMap.find n' g)) g
          
    let succs n m = NodeMap.find n m

    let fold f m acc = NodeMap.fold (fun node _ acc -> f node acc) m acc

    let remove_edge n m g =
    try
      let succs_n = NodeMap.find n g in
      let succs_m = NodeMap.find m g in
      let succs_n = NodeSet.remove m succs_n in
      let succs_m = NodeSet.remove n succs_m in
      let g = NodeMap.add n succs_n g in
      NodeMap.add m succs_m g
    with Not_found -> g

    let remove_node n g =
      let voisins = succs n g in
      let g = NodeSet.fold (fun n' acc ->
        remove_edge n n' acc
      ) voisins g in
      NodeMap.remove n g

  end

  module Graph = Make(Node)

(* ------------- IMPLEMENTATION DE L'ALGORITHME HILL CLIMBING -----------------*)

let random_choose node_set =
  let list = Graph.NodeSet.elements node_set in
  let i = Random.int (List.length list) in
  List.nth list i

(*  On a une liste de point visités, on commence par un point aléatoire.
    Pour chaque point du graphe, on relie aléatoirement à un point de la liste des points déjà visités.
    Si on a pas visité de point encore, on ajoute simplement ce point à la liste des points déjà visités*)
let build_candidate_tree g =
  let aux node vis acc =
    let vis' = Graph.NodeSet.add node vis in
    if Graph.NodeSet.is_empty vis then
      acc, vis'
    else
      let n = random_choose vis in
      Graph.add_edge node n acc, vis'
  in
  let g, _ = Graph.fold (fun node (acc, vis) -> aux node vis acc) g (g, Graph.NodeSet.empty) in
  g

let distance n1 n2 =
  let dx = n1.Node.x -. n2.Node.x in
  let dy = n1.Node.y -. n2.Node.y in
  sqrt (dx *. dx +. dy *. dy)

let size g =
  let aux node acc =
    let succs = Graph.succs node g in
    Graph.NodeSet.fold (fun succ acc ->
      acc +. distance node succ
    ) succs acc
  in
  (Graph.fold aux g 0.)/.2.

let add_relay g =
  let nodes = Graph.fold (fun node acc -> node :: acc) g [] in
  if (List.length nodes) < 3 then
    g
  else
  let rec choose_node nodes =
    let n = List.nth nodes (Random.int (List.length nodes)) in
    let n' = random_choose (Graph.succs n g) in
    if Graph.NodeSet.cardinal (Graph.succs n' g) > 1 then
      let rec choose_snd_node succs =
        let n'' = random_choose succs in
        if n'' = n then
          choose_snd_node succs
        else
          n''
      in
      let n'' = choose_snd_node (Graph.succs n' g) in
      n, n', n''
    else
      choose_node nodes
  in
  let n, n', n'' = choose_node nodes in
  let x_moy = (n.Node.x +. n'.Node.x +. n''.Node.x) /. 3. in
  let y_moy = (n.Node.y +. n'.Node.y +. n''.Node.y) /. 3. in
  let node = { Node.x = x_moy; Node.y = y_moy; Node.flag = false } in
  let g = Graph.add_node node g in
  let g = Graph.remove_edge n n' g in
  let g = Graph.remove_edge n' n'' g in
  let g = Graph.remove_edge n n'' g in
  let g = Graph.add_edge node n g in
  let g = Graph.add_edge node n' g in
  Graph.add_edge node n'' g

let fusion_relay g =
  let relay = Graph.fold (fun node acc ->
    if not node.Node.flag then
      node :: acc
    else
      acc
  ) g [] in
  if (List.length relay) < 1 then
    g
  else
    let r = List.nth relay (Random.int (List.length relay)) in
    let voisins = Graph.succs r g in
    let node = random_choose voisins in
    let g = Graph.NodeSet.fold (fun n acc ->
      if n = node then
        acc
      else
        let g = Graph.add_edge n node acc in
        Graph.remove_edge n r g
    ) voisins g
    in
    Graph.remove_node r g

let move_relay g =
  let relay = Graph.fold (fun node acc ->
    if not node.Node.flag then
      node :: acc
    else
      acc
  ) g [] in
  if (List.length relay) < 1 then
    g
  else
    let r = List.nth relay (Random.int (List.length relay)) in
    let max_x = Graph.fold (fun node acc -> max node.Node.x acc) g min_float in
    let min_x = Graph.fold (fun node acc -> min node.Node.x acc) g max_float in
    let max_y = Graph.fold (fun node acc -> max node.Node.y acc) g min_float in
    let min_y = Graph.fold (fun node acc -> min node.Node.y acc) g max_float in
    let dist_x = (max_x -. min_x) /. 5. in
    let dist_y = (max_y -. min_y) /. 5. in
    let x = r.Node.x +. (Random.float dist_x -. (dist_x /. 2.)) in
    let y = r.Node.y +. (Random.float dist_y -. (dist_y /. 2.)) in
    let r' = { Node.x = x; Node.y = y; Node.flag = false } in
    let g = Graph.add_node r' g in
    let voisins = Graph.succs r g in
    let g = Graph.remove_node r g in
    Graph.NodeSet.fold (fun n acc ->
      Graph.add_edge n r' acc
    ) voisins g

let random_action g =
  let i = Random.int 3 in
  if i = 0 then
    add_relay g
  else if i = 1 then
    move_relay g
  else
    fusion_relay g

(* ------------------ TEST DE L'IMPLEMENTATION ---------------------- *)

let setupgraph graph =
  (* Convertir un nœud en un point (x, y) de type float *)
  let node_to_point node =
    (node.Node.x, node.Node.y)
  in

  (* Extraire la liste des points (x, y) *)
  let points =
    Graph.fold (fun node acc ->
      if node.Node.flag then
        node_to_point node :: acc
      else
        acc
    ) graph []
  in

  let relay =
    Graph.fold (fun node acc ->
      if not node.Node.flag then
        node_to_point node :: acc
      else acc
    ) graph []
  in

  (* Extraire la liste des arêtes comme couples ((x1, y1), (x2, y2)) *)
  let edges =
    Graph.fold (fun node acc ->
      let succs = Graph.succs node graph in
      Graph.NodeSet.fold (fun succ acc ->
        let edge = (node_to_point node, node_to_point succ) in
        edge :: acc
      ) succs acc
    ) graph []
  in

  (points, relay, edges)

(*let test_steiner () =
  (* Taille de la fenêtre *)
  let sx, sy = 800, 600 in

  (* Créer un graphe d'exemple *)
  let coords = Input.read () in
  let _ = Input.dump coords in
  let g = Graph.empty in
  let g = List.fold_left (fun acc (x, y) ->
    let n = { Node.x = x; Node.y = y; Node.flag = true } in
    Graph.add_node n acc
  ) g coords in
  let g = build_candidate_tree g in
  let s = size g in
  Printf.printf "Taille du graphe : %f\n" s;
  (* Convertir le graphe en données affichables *)
  let (pts, relay, sol) = setupgraph g in

  (* Afficher le graphe *)
  let _ = Output.draw_steiner (sx, sy) pts relay sol in
  let g = add_relay g in
  let (pts, relay, sol) = setupgraph g in
  let _ = Output.draw_steiner (sx, sy) pts relay sol in
  let g = move_relay g in
  let (pts, relay, sol) = setupgraph g in
  let _ = Output.draw_steiner (sx, sy) pts relay sol in
  let g = fusion_relay g in
  let (pts, relay, sol) = setupgraph g in
  Output.draw_steiner (sx, sy) pts relay sol

let () = test_steiner ()*)

(* -------------- PROGRAMME PRINCIPAL ----------------*)

let steiner () =
  Random.self_init ();

  let sx, sy = 800, 600 in

  (* Créer un graphe d'exemple *)
  let coords = Input.read () in
  let _ = Input.dump coords in
  let g = Graph.empty in
  let g = List.fold_left (fun acc (x, y) ->
    let n = { Node.x = x; Node.y = y; Node.flag = true } in
    Graph.add_node n acc
  ) g coords in
  let g = build_candidate_tree g in
  let (pts, relay, sol) = setupgraph g in
  let _ = Output.draw_steiner (sx, sy) pts relay sol in
  let nb_iter = 10000 in
  let initial_size = size g in
  let rec loop g i =
    if i = nb_iter then
      g
    else
      let g' = random_action g in
      let s = size g in
      let s' = size g' in
      if s' < s then (
        (*Printf.printf "1 : Taille du graphe : %f\n" s';*)
        loop g' (i + 1)
      ) else (
        (*Printf.printf "2 : Taille du graphe : %f\n" s;*)
        loop g (i + 1)
      )
  in
  let g = loop g 0 in
  let final_size = size g in
  Printf.printf "Taille initiale : %f\n" initial_size;
  Printf.printf "Taille finale : %f\n" final_size;
  flush stdout;
  let (pts, relay, sol) = setupgraph g in
  Output.draw_steiner (sx, sy) pts relay sol

let () = steiner ()