structure Color : COLOR =
struct
  structure F = MipsFrame 
  structure T = Temp
  structure G = Graph

  structure GSet = RedBlackSetFn(
    struct
      type ord_key = G.node
      fun compare((_, a), (_, b)) = Int.compare(a, b)
    end)

  structure RSet = RedBlackSetFn(
    struct
      type ord_key = F.register
      fun compare(a, b) = String.compare(a, b)
    end)

  exception EmptySet
  exception ElementNotFound

  fun set l = GSet.addList(GSet.empty, l)

  (* intentionally throws exception if element not found *)
  fun unwrap (tab: 'a Graph.Table.table, e: G.node) =
    case Graph.Table.look(tab, e) of
      SOME(s) => s
    | NONE => raise ElementNotFound

  (* wrapper to retrieve an item from a set *)
  fun get_any s =
    case GSet.listItems s of
      [] => raise EmptySet
    | x::xs => x

  type allocation = F.register Temp.Table.table

  fun color {interference, initial, spillCost, registers} =
    let
      val K = List.length registers 
      val Liveness.IGRAPH({graph, tnode, gtemp, moves}) = interference

      (* Node work-lists, sets, and stacks *)
      val nodes = G.nodes graph
      val precolored = ref GSet.empty (* TODO: need to adapt from initial *)
      val simplifyWorklist = ref GSet.empty
      val spillWorklist = ref GSet.empty
      val spilledNodes = ref GSet.empty
      val coloredNodes = ref GSet.empty
      val selectStack: G.node Stack.stack ref = ref (Stack.new())

      (* Graph/auxilliary data structures *)
      (* TODO: remodel some of these data structures as functions instead *)
      val nodeColor: (F.register G.Table.table) ref = ref G.Table.empty
      val nodeDegree: (int G.Table.table) ref = ref G.Table.empty

      fun init_precolored node = 
        let
          val temp = gtemp node
        in
          case Temp.Table.look(initial, temp) of
            SOME(r) => 
              (precolored := GSet.add(!precolored, node);
              nodeColor := G.Table.enter(!nodeColor, node, r))
          | NONE => ()
        end

      fun degree n =
        case G.Table.look(!nodeDegree, n) of
          NONE => (print "node not found"; ~1)
        | SOME(d) => d

      fun makeWorklist () =
        let
          val nodes = G.nodes graph

          fun add_spill n = (spillWorklist := GSet.add(!spillWorklist, n))
          fun add_simplify n = (simplifyWorklist := GSet.add(!simplifyWorklist, n))
        in
          List.app (fn n => if (degree n) >= K then add_spill n else add_simplify n) nodes
        end

      fun adjacent n = 
        let
          val origAdjGSet = set (G.adj n)
          val selectGSet = set (Stack.members (!selectStack))
        in
          GSet.listItems (GSet.difference(origAdjGSet, selectGSet))
        end

      (* meant for when we have coalescing -- for now returns same node *)
      fun get_alias n = n

      fun decrementDegree n =
        let
          val SOME(curr_deg) = G.Table.look(!nodeDegree, n)
        in
          nodeDegree := G.Table.enter(!nodeDegree, n, curr_deg - 1)
        end

      fun simplify () =
        let
          val simplify_node = get_any (!simplifyWorklist)
        in
          simplifyWorklist := GSet.delete(!simplifyWorklist, simplify_node);
          selectStack := Stack.push(!selectStack, simplify_node);
          List.app decrementDegree (adjacent simplify_node)
        end

      (* note: we check that the worklist is not empty before calling this function *)
      fun spill () =
        let
          val spill_node = get_any (!spillWorklist)
        in
          spillWorklist := GSet.delete(!spillWorklist, spill_node);
          simplifyWorklist := GSet.add(!simplifyWorklist, spill_node)
        end

      fun assign_colors () =
        case Stack.isEmpty (!selectStack) of
          true => ()
        | false => 
            let
              val (new_stack, popped_node) = Stack.pop(!selectStack)
              (* TODO: change to register names? *)
              val okColors = ref (RSet.fromList registers)
              val neighbors = G.adj popped_node

              fun check_existing_colors w =
                let
                  val all_colored = GSet.union(!coloredNodes, !precolored) 
                  val orig_w = get_alias w
                in
                  if GSet.exists (fn x => G.eq(x, orig_w)) all_colored
                  then okColors := RSet.delete(!okColors, unwrap(!nodeColor, orig_w))
                  else ()
                end
            in
              print "assigning colors\n";
              selectStack := new_stack;
              List.app check_existing_colors neighbors;
              print (Int.toString(List.length (RSet.listItems (!okColors))) ^ "\n");
              case RSet.isEmpty (!okColors) of
                true => spilledNodes := GSet.add(!spilledNodes, popped_node)
              | false => coloredNodes := GSet.add(!coloredNodes, popped_node);
                         print ("can color with: " ^ hd (RSet.listItems (!okColors)) ^ "\n");
                         nodeColor := G.Table.enter(!nodeColor, 
                                                    popped_node, 
                                                    hd (RSet.listItems (!okColors)));
                         assign_colors()
            end

      fun main_loop () =
        if (GSet.isEmpty (!simplifyWorklist)) andalso (GSet.isEmpty (!spillWorklist))
        then assign_colors()
        else
          (case GSet.isEmpty (!simplifyWorklist) of
            true => ()
          | false => simplify();
           case GSet.isEmpty (!spillWorklist) of
            true => ()
          | false => spill();
          main_loop())
    in
      (* initial degrees *)
      nodeDegree := List.foldl (fn (n, tab) => G.Table.enter(tab, n, List.length (G.adj n)))
                 (!nodeDegree)
                 (G.nodes graph);
      List.app init_precolored nodes;
      makeWorklist();
      main_loop();
      let
        val temp_color =
          List.foldl (fn (n, tab) => 
                        Temp.Table.enter(tab, gtemp n, unwrap(!nodeColor, n)))
                      Temp.Table.empty
                      (G.nodes graph)

        val spills = List.map gtemp (GSet.listItems (!spilledNodes))
      in
        (temp_color, spills)
      end
    end
end