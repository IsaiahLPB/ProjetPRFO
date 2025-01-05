  (* ----------------- MODULES DE L'ALGORITHME HILL CLIMBING ----------------- *)
  
  module type OrderedType = sig
    type t
    val compare : t -> t -> int
  end

  module Node = struct
    type t = { x: float; y: float; flag: bool }

    (**
      @requires a valid node
      @ensures return the comparison between two nodes
    *)
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

    (**
      @requires Nothing
      @ensures return an empty graph
      *)
    let empty = NodeMap.empty

    (**
      @requires a valid graph
      @ensures return true if the graph is empty, false otherwise
      *)
    let is_empty m = NodeMap.is_empty m

    (**
      @requires a valid node and a valid graph
      @ensures return the graph with the added node
      *)
    let add_node n g =
        try
            let _ = NodeMap.find n g in
            g
        with
        | Not_found -> NodeMap.add n NodeSet.empty g

    (**
      @requires two valid nodes and a valid graph
      @ensures return the graph with an edge between n and n'*)
    let add_edge n n' g =
      let g = add_node n (add_node n' g) in
      let g = NodeMap.add n (NodeSet.add n' (NodeMap.find n g)) g in
      NodeMap.add n' (NodeSet.add n (NodeMap.find n' g)) g
    
    (**
      @requires a valid node and a valid graph
      @ensures return the successors of a node in the graph
    *)
    let succs n g = NodeMap.find n g

    (**
      @requires a valid function, a valid graph and an accumulator
      @ensures return the result of the function applied to the graph in the accumulator
    *)
    let fold f m acc = NodeMap.fold (fun node _ acc -> f node acc) m acc

    (**
      @requires two valid nodes and a valid graph
      @ensures return the graph without the edge between n and n'
    *)
    let remove_edge n m g =
    try
      let succs_n = NodeMap.find n g in
      let succs_m = NodeMap.find m g in
      let succs_n = NodeSet.remove m succs_n in
      let succs_m = NodeSet.remove n succs_m in
      let g = NodeMap.add n succs_n g in
      NodeMap.add m succs_m g
    with Not_found -> g

    (**
      @requires a valid node and a valid graph
      @ensures return the graph without the node n and its edges
    *)
    let remove_node n g =
      let voisins = succs n g in
      let g = NodeSet.fold (fun n' acc ->
        remove_edge n n' acc
      ) voisins g in
      NodeMap.remove n g

  end

  module Graph = Make(Node)

(* ------------- IMPLEMENTATION DE L'ALGORITHME HILL CLIMBING -----------------*)

(**
  @requires a valid Graph.NodeSet
  @ensures return a random node from the set
  Function build to ensure a true random choice during the execution
*)
let random_choose node_set =
  let list = Graph.NodeSet.elements node_set in
  let i = Random.int (List.length list) in
  List.nth list i

(**
  @requires a valid graph
  @ensures return a graph with a random candidate tree
  Function called once at the beginning of the main execution to build a random candidate tree
*)
let build_candidate_tree g =
  (*
    aux return a couple made of the graph with the added edge and the updated visited nodes.
    This structure was mandatory to ensure vis is updated at each iteration.
  *)
  let aux node vis acc =
    let vis' = Graph.NodeSet.add node vis in
    if Graph.NodeSet.is_empty vis then
      acc, vis'
      (* If node is the first node travelled, only add node to vis' *)
    else
      let n = random_choose vis in
      Graph.add_edge node n acc, vis'
  in
  let g, _ = Graph.fold (fun node (acc, vis) -> aux node vis acc) g (g, Graph.NodeSet.empty) in
  g

(**
  @requires two valid nodes
  @ensures return the distance between two nodes
*)
let distance n1 n2 =
  let dx = n1.Node.x -. n2.Node.x in
  let dy = n1.Node.y -. n2.Node.y in
  sqrt (dx *. dx +. dy *. dy)

(**
  @requires a valid graph
  @ensures return a float corresponding to the size of the graph
  Not optimized as we have to go through all the nodes and all the edges even if
  we've already been through some of them.
*)
let size g =
  let aux node acc =
    let succs = Graph.succs node g in
    Graph.NodeSet.fold (fun succ acc ->
      acc +. distance node succ
    ) succs acc
  in
  (Graph.fold aux g 0.)/.2.

(**
  @requires a valid graph
  @ensures return a graph with a new relay node
*)
let add_relay g =
  let nodes = Graph.fold (fun node acc -> node :: acc) g [] in
  (* If there's only two nodes in the graph we can't add a relay node *)
  if (List.length nodes) < 3 then
    g
  else
  (*
    Choose a random node n in the graph
    Choose a random node n' in the successors of n
    Check if n' has at least two successors, else there's no triangle to add a relay point
  *)
  let rec choose_node nodes =
    let n = List.nth nodes (Random.int (List.length nodes)) in
    let n' = random_choose (Graph.succs n g) in
    if Graph.NodeSet.cardinal (Graph.succs n' g) > 1 then
      (*
        Choose a random node n'' in the successors of n' and check if n'' is different from n
        If n'' is equal to n, choose a new n''
      *)
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
  let g = Graph.add_edge node n g in
  let g = Graph.add_edge node n' g in
  Graph.add_edge node n'' g

(**
  @requires a valid graph
  @ensures return a graph with a fusion of a relay node and a regular node
*)
let fusion_relay g =
  let relay = Graph.fold (fun node acc ->
    if not node.Node.flag then
      node :: acc
    else
      acc
  ) g []
  in

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

(**
  @requires a valid graph
  @ensures return a graph with a relay node moved
*)
let move_relay g =
  let relay = Graph.fold (fun node acc ->
    if not node.Node.flag then
      node :: acc
    else
      acc
  ) g []
  in

  if (List.length relay) < 1 then
    g
  else
    let r = List.nth relay (Random.int (List.length relay)) in
    let max_x = Graph.fold (fun node acc -> max node.Node.x acc) g min_float in
    let min_x = Graph.fold (fun node acc -> min node.Node.x acc) g max_float in
    let max_y = Graph.fold (fun node acc -> max node.Node.y acc) g min_float in
    let min_y = Graph.fold (fun node acc -> min node.Node.y acc) g max_float in
    (*
        dist_x and dist_y are the distances the relay node can move
        the division by ten is arbitrairy
    *)
    let dist_x = (max_x -. min_x) /. 10. in
    let dist_y = (max_y -. min_y) /. 10. in
    let x = r.Node.x +. (Random.float dist_x -. (dist_x /. 2.)) in
    let y = r.Node.y +. (Random.float dist_y -. (dist_y /. 2.)) in
    let r' = { Node.x = x; Node.y = y; Node.flag = false } in
    let g = Graph.add_node r' g in
    let voisins = Graph.succs r g in
    let g = Graph.remove_node r g in
    Graph.NodeSet.fold (fun n acc ->
      Graph.add_edge n r' acc
    ) voisins g

(**
  @requires a valid graph
  @ensures return a graph with a random action applied
*)
let random_action g =
  let i = Random.int 3 in
  if i = 0 then
    add_relay g
  else if i = 1 then
    move_relay g
  else
    fusion_relay g

(* ------------------ UTILE A LA VISUALISATION ---------------------- *)

(**
  @requires a valid graph
  @ensures return a tuple made of two lists of points (x, y) representing the regulars nodes, the relay nodes and a list of couple of points representing the edges
  Useful to draw the graph as the graph is not directly drawable for the output.ml file
*)
let setupgraph graph =
  (* Convert a node into a (float * float) *)
  let node_to_point node =
    (node.Node.x, node.Node.y)
  in

  (* Extract a list of all the regular nodes in a readable format *)
  let points =
    Graph.fold (fun node acc ->
      if node.Node.flag then
        node_to_point node :: acc
      else
        acc
    ) graph []
  in

  (* Extract a list of all the relay nodes in a readable format *)
  let relay =
    Graph.fold (fun node acc ->
      if not node.Node.flag then
        node_to_point node :: acc
      else acc
    ) graph []
  in

  (* Extract the list of all edges as a couple ((x1, y1), (x2, y2)) *)
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

(* -------------- PROGRAMME PRINCIPAL ----------------*)

(**
  @requires Nothing
  @ensures execute the hill climbing algorithm
*)
let steiner () =
  (* Init random *)
  Random.self_init ();

  let sx, sy = 1600, 1200 in

  (* Read the input given by the user and build the corresponding NodeSet *)
  let coords = Input.read () in
  let _ = Input.dump coords in
  let g = Graph.empty in
  let g = List.fold_left (fun acc (x, y) ->
    let n = { Node.x = x; Node.y = y; Node.flag = true } in
    Graph.add_node n acc
  ) g coords in

  (* Step one : build a random candidate tree *)
  let g = build_candidate_tree g in

  (* First draw *)
  let (pts, relay, sol) = setupgraph g in
  let _ = Output.draw_steiner (sx, sy) pts relay sol in

  (* Step two : apply the hill climbing algorithm *)
  let nb_iter = 10000 in
  let initial_size = size g in
  (* Main loop *)
  let rec loop g i =
    if i = nb_iter then
      g
    else
      let g' = random_action g in
      let s = size g in
      let s' = size g' in
      if s' < s then (
        loop g' (i + 1)
      ) else (
        loop g (i + 1)
      )
  in
  let g = loop g 0 in

  let final_size = size g in
  Printf.printf "Taille initiale : %f\n" initial_size;
  Printf.printf "Taille finale : %f\n" final_size;
  flush stdout;

  (* Draw the final graph *)
  let (pts, relay, sol) = setupgraph g in
  Output.draw_steiner (sx, sy) pts relay sol

let () = steiner ()