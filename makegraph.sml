structure MakeGraph :
sig
  val instrs2graph : Assem.instr list ->
                Flow.flowgraph * Flow.Graph.node list
end =

struct
  structure A = Assem
  fun instrs2graph instructs =
    let
      val g = Graph.newGraph()
      val def : Temp.temp list Graph.Table.table = Graph.Table.empty
      val use : Temp.temp list Graph.Table.table = Graph.Table.empty
      val ismove : bool Graph.Table.table = Graph.Table.empty
      val flow = {control=g, def=def, use=use, ismove=ismove}

      fun parse_instr_node (instr, {control, def, use, ismove}) =
        case instr of
          A.LABEL(_) => {control=control, def=def, use=use, ismove=ismove}
        | A.OPER({assem, dst, src, jump}) =>
            let
              val node = Graph.newNode(control)
            in
              (case jump of
                NONE => ()
              | SOME(ls) => (List.map (fn lab => let val target = Graph.newNode(control) 
                                                in Graph.mk_edge({from=node, to=target}) end)
                                      ls; ());
              {control=control,
               def=Graph.Table.enter(def, node, dst),
               use=Graph.Table.enter(use, node, src),
               ismove=Graph.Table.enter(ismove, node, false)})
            end
        | A.MOVE({assem, dst, src}) =>
            let
              val node = Graph.newNode(control)
            in
              {control=control,
               def=Graph.Table.enter(def, node, [dst]),
               use=Graph.Table.enter(use, node, [src]),
               ismove=Graph.Table.enter(ismove, node, true)}
            end
      
      val new_flow = List.foldl parse_instr_node flow instructs
    in
      (Flow.FGRAPH(new_flow), Graph.nodes (#control new_flow))
    end
    
end