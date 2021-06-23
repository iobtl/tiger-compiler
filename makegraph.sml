structure MakeGraph :
sig
  val instrs2graph : Assem.instr list ->
                Flow.flowgraph * Flow.Graph.node list
end =

struct
  structure A = Assem
  structure G = Graph

  (* constructs a control-flow graph from a sequence of instructions *)
  fun instrs2graph instructs =
    let
      val g = G.newGraph()

      val instr_nodes = List.map (fn i => (i, G.newNode(g))) instructs

      fun link_nodes_jump (instr, node) =
        let
          fun check_jump target =
            case List.find (fn (i, n) => case i of
                                          A.LABEL({lab, ...}) => lab = target
                                        | _ => false) instr_nodes
              of SOME((_, jump_node)) => G.mk_edge({from=node, to=jump_node})
        in
          case instr of
            A.OPER({jump=SOME(js), ...}) => List.app check_jump js
          | _ => ()
        end

      fun link_nodes_fall [] = ()
        | link_nodes_fall [(i, n)] = ()
        | link_nodes_fall ((i1, n1)::(i2, n2)::ns) =
          (case i1 of
            A.OPER({jump=SOME(j), ...}) => ()
          | _ => G.mk_edge({from=n1, to=n2}); 
          link_nodes_fall ns)

      fun parse_instruct ((A.OPER({dst, src, ...}), node), 
                          {control, def, use, ismove}) =
          {control=control,
           def=G.Table.enter(def, node, dst),
           use=G.Table.enter(use, node, src),
           ismove=G.Table.enter(ismove, node, false)}

        | parse_instruct ((A.MOVE({dst, src, ...}), node),
                          {control, def, use, ismove}) =
          {control=control,
           def=G.Table.enter(def, node, [dst]),
           use=G.Table.enter(use, node, [src]),
           ismove=G.Table.enter(ismove, node, true)}
        
        | parse_instruct ((A.LABEL(_), node),
                          {control, def, use, ismove}) =
          {control=control, def=def, use=use, ismove=ismove}


      val def : Temp.temp list G.Table.table = G.Table.empty
      val use : Temp.temp list G.Table.table = G.Table.empty
      val ismove : bool G.Table.table = G.Table.empty
    in
      List.app link_nodes_jump instr_nodes;
      link_nodes_fall instr_nodes;
      (Flow.FGRAPH(List.foldl parse_instruct {control=g, def=def, use=use, ismove=ismove} instr_nodes),
      Graph.nodes g)
    end
    
end