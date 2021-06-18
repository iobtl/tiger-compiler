structure MakeGraph :
sig
  val instrs2graph : Assem.instr list ->
                Flow.flowgraph * Flow.Graph.node list
end =

struct
  structure A = Assem
  datatype temp_node = TEMPNODE of {node: Graph.node, instruct: Assem.instr}

  fun instrs2graph instructs =
    let
      val g = Graph.newGraph()

      (* create headers for jump targets first so when creating edges to the
      same node we don't create a new node in the graph *)
      fun make_jump_targets (n, tab) =
        case n of
          A.LABEL({assem, lab}) => Symbol.enter(tab, lab, Graph.newNode(g))
        | _ => tab

      val jump_targets = List.foldl make_jump_targets Symbol.empty instructs

      fun parse_instruct (A.OPER({assem, dst, src, jump}), 
                          {control, def, use, ismove}) =
          let
            val node = Graph.newNode(control)
          in
            (case jump of
              NONE => ()
            | SOME(labs) => 
                List.app (fn lab => 
                          let val SOME(tar) = Symbol.look(jump_targets, lab)
                          in Graph.mk_edge({from=node, to=tar})
                          end)
                          labs);
            {control=control,
            def=Graph.Table.enter(def, node, dst),
            use=Graph.Table.enter(use, node, src),
            ismove=Graph.Table.enter(ismove, node, false)}
          end

        | parse_instruct (A.LABEL({assem, lab}), {control, def, use, ismove}) =
          {control=control, def=def, use=use, ismove=ismove}

        | parse_instruct (A.MOVE({assem, dst, src}), {control, def, use, ismove}) =
          let
            val node = Graph.newNode(control)
          in
            {control=control,
             def=Graph.Table.enter(def, node, [dst]),
             use=Graph.Table.enter(use, node, [src]),
             ismove=Graph.Table.enter(ismove, node, true)}
          end

      val def : Temp.temp list Graph.Table.table = Graph.Table.empty
      val use : Temp.temp list Graph.Table.table = Graph.Table.empty
      val ismove : bool Graph.Table.table = Graph.Table.empty
      val flow = {control=g, def=def, use=use, ismove=ismove}
      
      val new_flow = List.foldl parse_instruct flow instructs
    in
      (Flow.FGRAPH(new_flow), Graph.nodes (#control new_flow))
    end
    
end