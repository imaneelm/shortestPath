module type Graph =
  sig
    type node
    module NodeSet : Set.S with type elt = node
    type graph
    val empty : graph
    val is_empty : graph -> bool
    val succs : node -> graph -> NodeSet.t
    val add_vertex : node -> graph -> graph
    val add_edge : node -> node -> graph -> graph
    val remove_edge : node -> node -> graph -> graph
    (*val fold : (node -> 'a -> 'a) -> graph -> 'a -> 'a
    val vertex_nb : graph -> int
    val edge_nb : graph -> int
    val vertex_degree : node -> graph -> int
    val max_vertex_degree : graph -> node * int
    val transposed : graph -> graph
    val union : graph -> graph -> graph
    val intersection : graph -> graph -> graph
    val complementary : graph -> graph*)
  end


module MakeGen(N:Set.OrderedType)(NS:Set.S with type elt = N.t)(NM:Map.S with type key = N.t) : Graph with type node = N.t and module NodeSet = NS
     
module Make_g(N:Set.OrderedType):Graph with type node = N.t
