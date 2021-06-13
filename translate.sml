structure Translate : TRANSLATE = 
struct 
  structure F = MipsFrame
  structure T = Tree
  structure A = Absyn
  
  datatype level = Top
                 | Level of {parent: level, frame: F.frame} * unit ref
  type access = level * F.access

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

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

  val err = Ex(T.CONST(0))

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
    | unCx (Ex e) => (fn (t, f) => T.CJUMP(T.EQ, e, T.CONST(0), f, t))

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
    Ex(T.MEM(T.BINOP(T.PLUS, unEx base_addr, T.BINOP(T.MUL, sub_offset, T.CONST(F.wordSize)))))

  fun opExp (left, oper, right) =
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

    fun cond (left, oper ,right) =
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
    let
      datatype oper_type = ARITHMETIC | COMP
      val lefte = unEx left
      val righte = unEx right

      fun call_oper_func oper =
        case oper_type oper of
          ARITHMETIC => arithmetic(lefte, oper, righte)
        | COMP => comp(lefte, oper, righte)
    in
      call_oper_func oper
    end

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
        Nx(s) => Nx(seq(([test_func(t, f),
                          T.LABEL(t),
                          unNx then',
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(f),
                          unNx else',
                          T.JUMP(T.NAME(join), [join]), (* just fall through? *)
                          T.LABEL(join)])))
        Cx(f) => (* e.g. if a > b then c > d else 0 *)
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
        Nx(s) => Nx(seq(([test_func(t, f),
                          T.LABEL(t),
                          unNx s,
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(f),
                          T.JUMP(T.NAME(join), [join]),
                          T.LABEL(join)])))
    end
end
