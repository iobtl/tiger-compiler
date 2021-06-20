structure Liveness : LIVENESS =
struct
  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: (Temp.temp -> Graph.node),
                               gtemp: (Graph.node -> Temp.temp),
                               moves: (Graph.node * Graph.node) list}


  structure Set = RedBlackSetFn(
    struct
      type ord_key = Temp.temp
      fun compare(a, b) = String.compare(Temp.makestring a, Temp.makestring b)
    end)

  type liveSet = Set.set
  type liveMap = liveSet Flow.Graph.Table.table

  fun set l = Set.addList(Set.empty, l)

  fun list_unwrap (tab, n) =
    case Graph.Table.look(tab, n) of
      SOME(ls) => ls
    | NONE => []

  fun set_unwrap (tab, n) =
    case Graph.Table.look(tab, n) of
      SOME(s) => s
    | NONE => Set.empty

  fun interferenceGraph (Flow.FGRAPH({control, def, use, ismove})) =
    let
      val nodes = Graph.nodes control
      val ig = Graph.newGraph()
      val movelist: (Graph.node * Graph.node) list ref = ref []
      val init_lin = List.foldl (fn (n, tab) => Graph.Table.enter(tab, n, Set.empty))
                                 Graph.Table.empty nodes
      val init_lout = List.foldl (fn (n, tab) => Graph.Table.enter(tab, n, Set.empty))
                                  Graph.Table.empty nodes

      (* gets all temporaries in the original control-flow graph *)
      fun init_graph_temps nodes =
        let
          fun accumulate_temps (node, temps) =
            Set.addList(Set.addList(temps, list_unwrap(def, node)), 
                        list_unwrap(use, node))
        in
          Set.listItems (List.foldl accumulate_temps Set.empty nodes)
        end

      (* initializes nodes for interference graph; graph contains temporaries as nodes 
      and interferences as edges; returns tnode and gtemp mappings *)
      fun init_interf_mappings temps =
        let
          val init_tnode: Graph.node Temp.Table.table = Temp.Table.empty
          val init_gtemp: Temp.temp Graph.Table.table = Graph.Table.empty
          
          fun iter_temp (tnode, gtemp, t::ts) =
            (case Temp.Table.look(tnode, t) of
              SOME(node) => iter_temp(tnode, gtemp, ts) (* mapping already exists *)
            | NONE =>
                let
                  val new_node = Graph.newNode(ig)
                  val tnode' = Temp.Table.enter(tnode, t, new_node)
                  val gtemp' = Graph.Table.enter(gtemp, new_node, t)
                in
                  iter_temp(tnode', gtemp', ts)
                end)
            | iter_temp (tnode, gtemp, []) = (tnode, gtemp)
        in
          iter_temp(init_tnode, init_gtemp, temps)
        end

      fun tnode_map tab temp = 
        case Temp.Table.look(tab, temp) of
          SOME(n) => n

      fun gtemp_map tab node =
        case Graph.Table.look(tab, node) of 
          SOME(t) => t

      val (tnode, gtemp) = init_interf_mappings (init_graph_temps nodes)
      val tnode_mapping = tnode_map tnode
      val gtemp_mapping = gtemp_map gtemp

      (* iterative solution for dataflow equations *)
      (* repeat
          for each n
            in'[n] <- in[n]; out'[n] <- out[n]
            in[n] <- use[n] union (out[n]\def[n])
            out[n] <- Union(for all s in succ[n]) in[s]
         until in'[n] = in[n] and out'[n] = out[n] for all n *)
      fun iter_liveness (live_in: liveMap, live_out: liveMap): liveMap * liveMap =
        let
          val set_updated = ref false
          fun update_live_sets (node: Graph.node, (lin: liveMap, lout: liveMap)) =
            let
              val lin_set = set_unwrap(lin, node)
              val lout_set = set_unwrap(lout, node)
              val node_def = set(list_unwrap(def, node))
              val node_use = set(list_unwrap(use, node))

              val new_lin = Set.union(node_use, Set.difference(lout_set, node_def))
              val new_lout = List.foldl (fn (n, s) => Set.union(set_unwrap(lin, n), s))
                                         Set.empty (Graph.succ node)
            in
              if Set.equal(lin_set, new_lin) andalso Set.equal(lout_set, new_lout)
              then (lin, lout)
              else (set_updated := true;
                    (Graph.Table.enter(lin, node, new_lin),
                    Graph.Table.enter(lout, node, new_lout)))
            end

          val (final_in, final_out) = List.foldl update_live_sets (live_in, live_out) (List.rev nodes)
        in
          if !set_updated
          then iter_liveness(final_in, final_out)
          else (final_in, final_out)
        end

      val (live_in, live_out) = iter_liveness(init_lin, init_lout)

      (* adding interference edges *)
      fun add_interf_edges node =
        let
          val node_def = list_unwrap(def, node)
          val node_lout = Set.listItems (set_unwrap(live_out, node))
          val node_ismove = case Graph.Table.look(ismove, node) of
            NONE => false
          | SOME(t) => t

          (* 1. For nonmove instructions that define a variable _a_, with live-out variables
             b1, ..., bn, add interference edges (a, b1), ... 
             2. For move instructions a <- c, with live-out variables b1, ..., bn, add
             interference edges (a, b1), ... for any b that is not c*)
          fun add_nonmove (from, to) = Graph.mk_edge({from=from, to=to})
          fun add_move (from, to) = (movelist := (from, to)::(!movelist);
                                    if not (Graph.eq(from, to)) 
                                    then Graph.mk_edge({from=from, to=to})
                                    else ())
        in
          if node_ismove
          then List.app (fn defn => 
                          (List.app (fn outn => add_nonmove(tnode_mapping defn, tnode_mapping outn))
                                     node_lout)) 
                        node_def
          else List.app (fn defn =>
                          (List.app (fn outn => add_move(tnode_mapping defn, tnode_mapping outn))
                                     node_lout))
                        node_def
        end
    in
      List.app add_interf_edges nodes;
      (IGRAPH({graph=ig, 
               tnode=tnode_mapping, 
               gtemp=gtemp_mapping, 
               moves=(!movelist)}),
      fn n => Set.listItems (set_unwrap(live_out, n)))
    end


  (* for debugging; prints out list of nodes in igraph, and for each node, a list
  of nodes adjacent to it *)
  fun show (out, ig) =
    let
      val IGRAPH({graph, tnode, gtemp, moves}) = ig

      fun print_adj node =
        let
          val adj_nodes = Graph.adj node
        in
          (TextIO.output(out, ((Graph.nodename node) ^ " has adjacent nodes: \n"));
          List.app (fn n => TextIO.output(out, ((Graph.nodename n) ^ "\n"))) adj_nodes;
          TextIO.output(out, "----------"))
        end
    in
      List.app print_adj (Graph.nodes graph)
    end
end