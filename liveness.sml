structure Liveness : LIVENESS =
struct
  datatype igraph = IGRAPH of {graph: IGraph.graph,
                               tnode: (Temp.temp -> IGraph.node),
                               gtemp: (IGraph.node -> Temp.temp),
                               moves: (IGraph.node * IGraph.node) list}
end