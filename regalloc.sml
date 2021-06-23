structure RegAlloc : REG_ALLOC =
struct
  structure F = MipsFrame : FRAME
  structure T = Tree
  structure A = Assem

  type allocation = F.register Temp.Table.table

  (* returns final assembly program and register-temporary allocation *)
  fun alloc (instrs, frame): A.instr list * allocation =
    let
      val (fgraph, _) = MakeGraph.instrs2graph instrs
      val (igraph, _) = Liveness.interferenceGraph fgraph

      (* naive spillCost *)
      fun spillCost n = 1

      val (color_alloc, spills) = Color.color({interference=igraph,
                                               initial=F.tempMap,
                                               spillCost=spillCost,
                                               registers=F.registers})

      fun rewrite_program spills =
        let
          fun handle_spill spill =
            let
              val mem_offset = F.exp (F.allocLocal frame true) (T.TEMP(F.FP))

              (* insert store after each definition of new temp *)
              fun alloc_def (defls, t) =
                if List.exists (fn def => def = t) defls
                then MipsGen.codegen frame (T.MOVE(mem_offset, T.TEMP(t)))
                else []

              (* insert fetch before each use of new temp *)
              fun alloc_use (usels, t) =
                if List.exists (fn use => use = t) usels
                then MipsGen.codegen frame (T.MOVE(T.TEMP(t), mem_offset))
                else []

              fun check_instr instr =
                case instr of
                  A.OPER({assem, dst, src, ...}) =>
                    let
                      val store_temp = alloc_def(dst, spill)
                      val fetch_temp = alloc_use(src, spill)
                    in
                      store_temp@[instr]@fetch_temp
                    end
                | A.MOVE({assem, dst, src}) =>
                    let
                      val store_temp = alloc_def([dst], spill)
                      val fetch_temp = alloc_use([src], spill)
                    in
                      store_temp@[instr]@fetch_temp
                    end
                | A.LABEL({assem, lab}) => [instr]

              fun rewrite_instrs (instrs, new_instr): A.instr list =
                case instrs of
                  instr::instrs' => rewrite_instrs(instrs', (check_instr instr)@new_instr)
                | [] => new_instr
            in
              List.rev (rewrite_instrs(instrs, []))
            end
        in
          List.foldl (fn (s, instrs) => handle_spill s) instrs spills
        end
    in
      case spills of
        [] => (instrs, color_alloc)
      | _ => alloc(rewrite_program spills, frame)
    end
end