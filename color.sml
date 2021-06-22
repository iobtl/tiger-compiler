structure Color : COLOR =
struct
  structure F = MipsFrame : FRAME
  structure T = Temp
  structure G = Graph

  structure Set = RedBlackSetFn(
    struct
      type ord_key = Graph.node
      fun compare((_, a), (_, b)) = Int.compare(a, b)
    end)

  fun set l = Set.addList(Set.empty, l)

  fun list_unwrap (tab, n) =
    case Graph.Table.look(tab, n) of
      SOME(ls) => ls
    | NONE => []

  fun set_unwrap (tab, n) =
    case Graph.Table.look(tab, n) of
      SOME(s) => s
    | NONE => Set.empty

  type allocation = Frame.register Temp.Table.table

  (* Node work-lists, sets, and stacks *)
  val precolored = ref Set.empty
  val initial = ref Set.empty
  val simplifyWorklist = ref Set.empty
  val spillWorklist = ref Set.empty
  val spilledNodes = ref Set.empty
  val coloredNodes = ref Set.empty
  val selectStack = ref Stack.new()

  (* Graph/auxilliary data structures *)
  (* TODO: remodel some of these data structures as functions instead *)
  val adjSet: (G.node * G.node) list = ref [] (* TODO: better as set? *)
  val adjList: Set.set G.Table.table = ref G.Table.empty
  (*val moveList: Assem.instr G.Table.table = ref G.Table.empty*)
  val nodeColor: F.register G.Table.table = ref G.Table.empty
  val nodeDegree: int G.Table.table = ref G.Table.empty

  fun color ({interference, initial, spillCost, registers}) =
    let
      val K = List.length registers
      val Liveness.IGRAPH({graph, tnode, gtemp, moves}) = interference

      nodeDegree := (List.foldl (fn (n, tab) => G.Table.enter(tab, n, List.length (G.adj n)))
                 !nodeDegree
                 G.nodes graph)

      (*fun build () = *)
      
      fun addEdge (u, v) =

      fun degree n =
        case G.Table.look(!nodeDegree, n) of
          NONE => print "node not found"; -1
        | SOME(d) => d

      fun makeWorklist () =
        let
          val nodes = G.nodes graph

          fun add_spill n = (spillWorklist := Set.add(!spillWorklist, n))
          fun add_simplify n = (simplifyWorklist := Set.add(!simplifyWorklist, n))
        in
          List.app (fn n => if (degree n) >= K then add_spill n else add_simplify n) nodes
        end

      (* TODO: need to use this to simulate nodes which are 'removed' from graph *)
      fun adjacent n =

      (*fun nodeMoves () =

      fun moveRelated () =*)

      fun simplify () =
        let
          val simplify_node = Set.minItem !simplifyWorklist
        in
          (simplifyWorklist := Set.delete(!simplifyWorklist, simplify_node);
           selectStack := Stack.push(!selectStack, simplify_node);
           List.app decrementDegree (G.adj simplify_node))
        end

      fun decrementDegree n =
        let
          val SOME(curr_deg) = G.Table.look(!nodeDegree, n)
        in
          nodeDegree := G.Table.enter(!nodeDegree, n, curr_deg - 1)
        end

      fun enableMoves ns =

      fun addWorklist wl =

      fun ok (n1, n2) =

      fun conservative ns =

      fun getAlias n =

      fun combine (u, v) =

      fun selectSpill () =

      fun assignColors () =

      fun rewriteProgram () =
    in
      body
    end
end