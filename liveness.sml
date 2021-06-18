structure Liveness : LIVENESS =
struct
  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: (Temp.temp -> Graph.node),
                               gtemp: (Graph.node -> Temp.temp),
                               moves: (Graph.node * Graph.node) list}
  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet Flow.Graph.Table.table


  fun interferenceGraph Flow.FGRAPH({control, def, use, ismove}) =
    let
      
    in
      body
    end

  (* for debugging; prints out list of nodes in igraph, and for each node, a list
  of nodes adjacent to it *)
  fun show (out, ig) =
    let
      val IGRAPH({graph, tnode, gtemp, moves}) = ig

      fun get_adj node = (node, Graph.adj node)
      fun print_adj node =
        let
          val adj_nodes = get_adj node
        in
          (print (Graph.nodename node) ^ " has adjacent nodes: \n";
          List.map (fn n => print (Graph.nodename n) ^ "\n") adj_nodes;
          print "----------")
        end
    in
      List.map print_adj (Graph.nodes graph)
    end
end