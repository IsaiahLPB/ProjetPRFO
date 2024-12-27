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
    Printf.printf "Graphe vide : %b\n" (Graph.is_empty g);

    (* Itérer sur les nœuds *)
    Graph.fold (fun node acc ->
      Printf.printf "Nœud: (x=%.1f, y=%.1f, flag=%b)\n"
        node.Node.x node.Node.y node.Node.flag;
      acc
    ) g ()


(* ----------------- IMPLEMENTATION DE GRAPHICS ----------------- *)
(*
module IntSet = Set.Make(struct type t = int let compare = (-) end)


let draw_edge ((x1,y1),(x2,y2)) =
    let _ = Graphics.moveto x1 y1 in
    Graphics.lineto x2 y2

let draw_pb pts =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.black in
    let _ = List.iter (fun (x,y) -> Graphics.fill_circle x y 5) pts in
    Graphics.set_color c

let draw_sol sol =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.red in
    let _ = Graphics.set_line_width 3 in
    let _ = List.iter draw_edge sol in
    let _ = Graphics.set_line_width 1 in
    Graphics.set_color c

let draw (sx,sy) pts sol =
    let args = Printf.sprintf " %dx%d" sx sy in
    let _ = Graphics.open_graph args in
    let _ = draw_pb pts in
    let _ = draw_sol sol in
    let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
    Graphics.close_graph ()


let draw_steiner (sx,sy) pts sol =
    let ps0 = pts in
    let ps  = List.fold_left (fun acc (c1,c2) -> c1::c2::acc) ps0 sol in
    let xm  = List.fold_left (fun acc (x,_) -> min acc x) infinity ps in
    let xM  = List.fold_left (fun acc (x,_) -> max acc x) neg_infinity ps in
    let ym  = List.fold_left (fun acc (_,y) -> min acc y) infinity ps in
    let yM  = List.fold_left (fun acc (_,y) -> max acc y) neg_infinity ps in
    let margin  = 10.0 in
    let sxf = float_of_int sx in
    let syf = float_of_int sy in
    let _ = if sxf <= 2.0 *. margin
            then failwith "[draw_steiner] width must be more than 20px."
            else ()
    in
    let _ = if syf <= 2.0 *. margin
            then failwith "[draw_steiner] height must be more than 20px."
            else ()
    in
    let eps     = 1e-6 in
    let zoom_x  = max eps ((sxf -. 2.0 *. margin) /. (max eps (xM -. xm))) in
    let zoom_y  = max eps ((syf -. 2.0 *. margin) /. (max eps (yM -. ym))) in
    let adjust (x,y) =
        ( int_of_float ( margin +. zoom_x *. (x-.xm))
        , int_of_float ( margin +. zoom_y *. (y-.ym))
        )
    in
    let pts' = List.map adjust ps0 in
    let sol' = List.map (fun (c1,c2) -> (adjust c1, adjust c2)) sol in
    draw (sx,sy) pts' sol'
    *)