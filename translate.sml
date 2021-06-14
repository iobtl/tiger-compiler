structure Translate : TRANSLATE = 
struct 
  structure F : FRAME = MipsFrame
  structure T = Tree
  structure A = Absyn
  
  datatype level = Top
                 | Level of {parent: level, frame: F.frame} * unit ref
  type access = level * F.access

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  type frag = F.frag

  val frag_list : frag list ref = ref []
  fun seq [x] = x
    | seq (x::x'::[]) = T.SEQ(x, x')
    | seq (x::x'::xs) = T.SEQ(x, T.SEQ(x', seq xs))

  val outermost = Top
  fun newLevel {parent, name, formals} =
    Level({parent=parent, frame=F.newFrame({name=name, formals=true::formals})}, ref())

  fun formals level =
    case level of
      Top => nil
    | Level({parent, frame}, _) => List.map (fn x => (level, x)) (F.formals frame) (* return without static link? *)

  fun allocLocal (level : level) esc =
    case level of
      Level({parent, frame}, _) => (level, F.allocLocal frame esc)

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
      let
        val r = Temp.newtemp()
        val t = Temp.newlabel() 
        val f = Temp.newlabel()
      in
        T.ESEQ(seq([T.MOVE(T.TEMP(r), T.CONST(1)),
                    genstm(t, f),
                    T.LABEL(f),
                    T.MOVE(T.TEMP(r), T.CONST(0)),
                    T.LABEL(t)]),
               T.TEMP(r))
      end
    | unEx (Nx s) = T.ESEQ(s, T.CONST(0))

  fun unNx (Ex e) = T.EXP(e)
    | unNx (Cx genstm) =
      let
        val t = Temp.newlabel() and f = Temp.newlabel()
      in
        genstm(t, f)
      end
    | unNx (Nx s) = s

  fun unCx (Cx s) = s
    | unCx (Ex (T.CONST(0))) = (fn (t, f) => T.JUMP(T.NAME(f), [f]))
    | unCx (Ex (T.CONST(1))) = (fn (t, f) => T.JUMP(T.NAME(t), [t]))
    | unCx (Ex e) = (fn (t, f) => T.CJUMP(T.EQ, e, T.CONST(0), f, t))

  val err = Ex(T.CONST(0))
  val nilexp = Ex(T.CONST(0))

  fun intExp i = Ex(T.CONST(i))

  fun stringExp s = 
    let
      val lab = Temp.newlabel()
    in
      frag_list := (F.STRING(lab, s))::(!frag_list);
      Ex(T.NAME(lab))
    end

  fun simpleVar (access, level) =
    let
      val (Level(_, dec_ref), dec_access) = access

      fun join_link (fp, level) =
        let
          val Level({parent=curr_parent, frame=curr_frame}, curr_ref) = level
          val static_link = hd (F.formals curr_frame)
        in
          if curr_ref = dec_ref
          then F.exp dec_access fp
          else join_link(F.exp static_link fp, curr_parent)
        end
    in
      Ex(join_link(T.TEMP(F.FP), level))
    end

  fun fieldVar (base_addr, field, fieldls) =
    let
      fun index xs acc =
        let
          val (x::xs') = xs
        in
          if x = field
          then acc
          else index xs' (acc+1)
        end
    in
      Ex(T.MEM(T.BINOP(T.PLUS, unEx base_addr, T.BINOP(T.MUL, T.CONST(index fieldls 0), T.CONST(F.wordSize)))))
    end

  fun subscriptVar (base_addr, sub_offset) =
    Ex(T.MEM(T.BINOP(T.PLUS, unEx base_addr, T.BINOP(T.MUL, unEx sub_offset, T.CONST(F.wordSize)))))

  fun opExp (left, oper, right) =
    let
      fun arithmetic (left, oper, right) =
        let
          fun oper_ir oper =
            case oper of
              A.PlusOp => T.PLUS
            | A.MinusOp => T.MINUS
            | A.TimesOp => T.MUL
            | A.DivideOp => T.DIV 
        in
          Ex(T.BINOP(oper_ir oper, left, right))
        end

      fun comp (left, oper ,right) =
        let
          fun oper_ir oper =
            case oper of
              A.EqOp => T.EQ
            | A.NeqOp => T.NE
            | A.LtOp => T.LT
            | A.LeOp => T.LE
            | A.GtOp => T.GT
            | A.GeOp => T.GE
        in
          Cx((fn (t, f) => T.CJUMP(oper_ir oper, left, right, t, f)))
        end

      val lefte = unEx left
      val righte = unEx right

      datatype oper_type = ARITHMETIC | COMP

      fun get_oper_type A.PlusOp = ARITHMETIC
        | get_oper_type A.MinusOp = ARITHMETIC
        | get_oper_type A.TimesOp = ARITHMETIC
        | get_oper_type A.DivideOp = ARITHMETIC
        | get_oper_type A.EqOp = COMP
        | get_oper_type A.NeqOp = COMP
        | get_oper_type A.LtOp = COMP
        | get_oper_type A.LeOp = COMP
        | get_oper_type A.GtOp = COMP
        | get_oper_type A.GeOp = COMP

      fun call_oper_func oper =
        case get_oper_type oper of
          ARITHMETIC => arithmetic(lefte, oper, righte)
        | COMP => comp(lefte, oper, righte)

    in
      call_oper_func oper
    end

  fun assignExp (var, exp) = Nx(T.MOVE(unEx var, unEx exp))

  fun ifThenElseExp (test, then', else') =
    let
      val test_func = unCx test
      val r = Temp.newtemp()
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val join = Temp.newlabel()
    in
      case then' of (* then' and else' must have the same type; type-checked in semant *)
        Ex(e) => Ex(T.ESEQ(seq([test_func(t, f),
                                T.LABEL(t),
                                T.MOVE(T.TEMP(r), unEx then'),
                                T.JUMP(T.NAME(join), [join]),
                                T.LABEL(f),
                                T.MOVE(T.TEMP(r), unEx else'),
                                T.JUMP(T.NAME(join), [join]),
                                T.LABEL(join)]),
                           T.TEMP(r)))
      | Nx(s) => Nx(seq([test_func(t, f),
                          T.LABEL(t),
                          unNx then',
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(f),
                          unNx else',
                          T.JUMP(T.NAME(join), [join]), (* just fall through? *)
                          T.LABEL(join)]))
      | Cx(c) => (* e.g. if a > b then c > d else 0 *)
          let
            val y = Temp.newlabel()
            val z = Temp.newlabel()
          in
            Cx(fn (t', f') =>
                seq([test_func(t, f),
                     seq([T.LABEL(t),
                          (unCx then') (t', f'),
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(f),
                          (unCx else') (t', f'),
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(join)])]))
          end
    end

  fun ifThenExp (test, then') =
    let
      val test_func = unCx test
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val join = Temp.newlabel()
    in
      case then' of (* by convention, if no 'else', result of whole if-then is unit (T.stm) *)
        Nx(s) => Nx(seq([test_func(t, f),
                          T.LABEL(t),
                          s,
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(f),
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(join)]))
    end

  fun callExp (label, args, caller_level, callee_level, is_unit) =
    (* need to obtain static link of callee function from caller_level *)
    let
      fun comp_depth Top = 0
        | comp_depth (Level({parent, frame}, _)) = 1 + comp_depth parent

      fun find_link (diff, level) =
        let
          val Level({parent, frame}, _) = level
        in
          case diff of
            0 => T.TEMP(F.FP) (* caller function is direct parent of callee function *)
          | _ => F.exp (hd (F.formals frame)) (find_link(diff-1, parent))
        end

      (* functions can only be used at depths >= their defined level + 1 *)
      val nest_diff = (comp_depth caller_level) - (comp_depth callee_level) + 1
      val res = T.CALL(T.NAME(label), (find_link(nest_diff, caller_level))::(List.map unEx args))
    in
      if is_unit
      then Nx(T.EXP(res))
      else Ex(res)
    end

  fun whileExp (test, body, break) =
    let
      val test_lab = Temp.newlabel()
      val body_lab = Temp.newlabel()
    in
      Nx(seq([T.LABEL(test_lab),
              T.CJUMP(T.EQ, unEx test, T.CONST(1), body_lab, break),
              T.LABEL(body_lab),
              unNx body,
              T.JUMP(T.NAME(test_lab), [test_lab]),
              T.LABEL(break)])) (* evaluate condition to expression value *)
    end

  fun forExp (lo, hi, body, break) =
    let
      val test_lab = Temp.newlabel()
      val body_lab = Temp.newlabel()
      val inc_lab = Temp.newlabel()
      val i = Temp.newtemp()
    in
      Nx(seq([T.MOVE(T.TEMP(i), unEx lo),
              T.LABEL(test_lab),
              T.CJUMP(T.LE, T.TEMP(i), unEx hi, body_lab, break),
              T.LABEL(body_lab),
              unNx body,
              T.CJUMP(T.LT, T.TEMP(i), unEx hi, inc_lab, break), (* account for hi = maxint *)
              T.LABEL(inc_lab),
              T.MOVE(T.TEMP(i), T.BINOP(T.PLUS, T.TEMP(i), T.CONST(1))),
              T.JUMP(T.NAME(test_lab), [test_lab]),
              T.LABEL(break)]))
    end

  (* need to initialize individual fields as well *)
  fun recordExp fields =
    Ex(F.externalCall("allocRecord", [T.CONST((List.length fields) * F.wordSize)]))
   
  fun breakExp break =
    Nx(T.JUMP(T.NAME(break), [break]))

  fun letExp (assignls, body) =
    Ex(T.ESEQ(seq(List.map unNx assignls), unEx body))

  fun arrayExp (size, init) =
    Ex(F.externalCall("initArray", [unEx size, unEx init]))

  fun procEntryExit ({level=Level({parent, frame}, _), body}) =
    let
      val ret = F.procEntryExit1(frame, T.MOVE(T.TEMP(F.RV), unEx body))
    in
      frag_list := F.PROC({body=ret, frame=frame})::(!frag_list); ()
    end
  
  fun getResult () = (!frag_list)
end
