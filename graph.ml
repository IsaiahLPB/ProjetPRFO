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
  end

  module Graph = Make(Node)

(* --------------- TEST D'UNE IMPLEMENTATION --------------- *)
(*
  let () = (* Sert de test pour voir si ça fonctionne *)
    (* Créer un graphe vide *)
    let g = Graph.empty in

    (* Définir des nœuds *)
    let n1 = { Node.x = 1.0; Node.y = 2.0; Node.flag = true } in
    let n2 = { Node.x = 3.0; Node.y = 4.0; Node.flag = false } in
    let n3 = { Node.x = 5.0; Node.y = 6.0; Node.flag = true } in

    (* Ajouter des nœuds et des arêtes *)
    let g = Graph.add_edge n1 n2 g in
    let g = Graph.add_edge n2 n3 g in

    (* Vérifier si le graphe est vide *)
    Printf.printf "Graphes vide : %b\n" (Graph.is_empty g);

    (* Itérer sur les nœuds *)
    Graph.fold (fun node acc ->
      Printf.printf "Nœud: (x=%.1f, y=%.1f, flag=%b)\n"
        node.Node.x node.Node.y node.Node.flag;
      acc
    ) g ()
*)

(* ----------------- IMPLEMENTATION DE INPUT ----------------- *)
(* a l'air de marcher
let () =
  let coords = Input.read () in
  let _ = Input.dump coords in
  let g = Graph.empty in
  let g = List.fold_left (fun acc (x, y) ->
    let n = { Node.x = x; Node.y = y; Node.flag = true } in
    Graph.add_node n acc
  ) g coords in
  let g = Graph.add_edge { Node.x = 1.0; Node.y = 2.0; Node.flag = true } { Node.x = 3.0; Node.y = 4.0; Node.flag = true } g in
  let g = Graph.add_edge { Node.x = 3.0; Node.y = 4.0; Node.flag = true } { Node.x = 4.0; Node.y = 2.0; Node.flag = true } g in
  (* Vérifier si le graphe est vide *)
  Printf.printf "ntm vide : %b\n" (Graph.is_empty g);
  Graph.fold (fun node acc ->
      Printf.printf "Nœudouais: (x=%.1f, y=%.1f, flag=%b)\n"
        node.Node.x node.Node.y node.Node.flag;
      acc
    ) g ()
*)
(* ------------------ TEST DE L'IMPLEMENTATION ---------------------- *)

let setupgraph graph =
  (* Convertir un nœud en un point (x, y) de type float *)
  let node_to_point node =
    (node.Node.x, node.Node.y)
  in

  (* Extraire la liste des points (x, y) *)
  let points =
    Graph.fold (fun node acc ->
      node_to_point node :: acc
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

  (points, edges)

let () =
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
  let g = Graph.add_edge { Node.x = 1.0; Node.y = 2.0; Node.flag = true } { Node.x = 3.0; Node.y = 4.0; Node.flag = true } g in
  let g = Graph.add_edge { Node.x = 3.0; Node.y = 4.0; Node.flag = true } { Node.x = 4.0; Node.y = 2.0; Node.flag = true } g in

  (* Convertir le graphe en données affichables *)
  let (pts, sol) = setupgraph g in

  (* Afficher le graphe *)
  Output.draw_steiner (sx, sy) pts sol