module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

type node = { x: float; y: float; flag: bool }

(* Implémentation de OrderedType pour les nœuds *)
module Node : OrderedType =
struct
  type t = { x: float; y: float; flag: bool }      (* Check si ça marche avec jude "node"*)

  let compare n1 n2 =
    let c1 = Float.compare n1.x n2.x in
    if not (c1 = 0) then c1
    else
      let c2 = Float.compare n1.y n2.y in
      if not (c2 = 0) then c2
      else Bool.compare n1.flag n2.flag
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