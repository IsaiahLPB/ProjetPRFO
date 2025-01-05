module type Graph = sig
  type node
  module NodeSet : Set.S with type elt = node
  type graph
  val empty : graph
  val is_empty : graph -> bool
  val add_node : node -> graph -> graph
  val add_edge : node -> node -> graph -> graph
  val succs : node -> graph -> NodeSet.t
  val fold : (node -> 'a -> 'a) -> graph -> 'a -> 'a
  val remove_edge : node -> node -> graph -> graph
  val remove_node : node -> graph -> graph
  (*val coord : node -> (int * int)
  val extract_point : graph -> (int * int) list
  val extract_ledge : graph -> (node * node) list*)
end

module Make(N:Set.OrderedType) : Graph with type node = N.t