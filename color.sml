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

  fun unwrap (tab: 'a Graph.Table.table, e: Graph.node) =
    case Graph.Table.look(tab, e) of
      NONE => print "no value found in table"
    | SOME(s) => s

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
      val K = List.length registers (* TODO: map K integers to colors for easier interfacing? *)
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

      fun adjacent n = 
        let
          val origAdjSet = G.adj n
          val selectSet = set (Set.members !selectStack)
        in
          Set.listItems (Set.difference(origAdjSet, selectSet))
        end

      (*fun nodeMoves () =

      fun moveRelated () =*)

      fun simplify () =
        let
          val simplify_node = Set.minItem !simplifyWorklist
        in
          simplifyWorklist := Set.delete(!simplifyWorklist, simplify_node);
          selectStack := Stack.push(!selectStack, simplify_node);
          List.app decrementDegree (adjacent simplify_node)
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

      (* meant for when we have coalescing -- for now returns same node *)
      fun get_alias n = n

      fun combine (u, v) =

      (* note: we check that the worklist is not empty before calling this function *)
      fun spill () =
        let
          val spill_node = Set.minItem !spillWorklist
        in
          spillWorklist := Set.delete(!spillWorklist, spill_node);
          simplifyWorklist := Set.add(!simplifyWorklist, spill_node)
        end

      fun assign_colors () =
        case Set.isEmpty !selectStack of
          true => ()
        | false => 
            let
              val (new_stack, popped_node) = Set.pop(!selectStack)
              val okColors = ref (set List.tabulate(K, (fn x => x)))

              fun check_existing_colors w =
                let
                  val all_colored = Set.union(!coloredNodes, !precolored) 
                  val orig_w = get_alias w
                in
                  if Set.exists (fn x => G.eq(x, orig_w)) all_colored
                  then okColors := Set.delete(!okColors, unwrap(tab, orig_w))
                  else ()
                end
            in
              selectStack := new_stack;
              case Set.isEmpty !okColors of
                true => spilledNodes := Set.add(!spilledNodes, popped_node)
              | false => coloredNodes := Set.add(!coloredNodes, popped_node);
                         nodeColor := G.Table.enter(!nodeColor, 
                                                    popped_node, 
                                                    Set.minItem !okColors);
            end

      fun rewriteProgram () =

      fun main_loop () =
        if (Set.isEmpty !simplifyWorklist) andalso (Set.isEmpty !spillWorklist)
        then assign_colors()
        else
          (case Set.isEmpty !simplifyWorklist of
            true => ()
          | false => simplify());
          (case Set.isEmpty !spillWorklist of
            true => ()
          | false => spill());
          main_loop()
    in
      makeWorklist();
      main_loop();
      if not (Set.isEmpty !spilledNodes)
      then rewrite_program !spilledNodes; color({interference=interference,
                                                 initial=initial,
                                                 spillCost=spillCost,
                                                 registers=registers})
      else ()
    end
end