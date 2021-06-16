signature CODEGEN =
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure A = Assem

  fun codegen frame (stm: Tree.stm) : A.instr list =
    let
      val ilist = ref (nil: A.instr list)
      fun emit x = ilist := x::(!ilist)
      fun int i = if i < 0 then Integer.toString(~i) else Integer.toString(i)

      fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b);

        (* sw $1,c($2): copy from register to memory *)
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST(i))), e2)) =
            emit(A.OPER({assem="sw 's1," ^ int i ^ "('s0)\n" 
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(i), e1)), e2)) =
            emit(A.OPER({assem="sw 's1," ^ int i ^ "('s0)\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST(i))), e2)) =
            emit(A.OPER({assem="sw 's1,-" ^ int i ^ "('s0)\n" 
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, T.CONST(i), e1)), e2)) =
            emit(A.OPER({assem="sw 's1,-" ^ int i ^ "('s0)\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER({assem="sw 's1,0('s0)",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))

        (* lw $1,c($2): copy from memory to register *)
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.PLUS, T.CONST(n), e1)))) =
            emit(A.OPER({assem="lw 'd0," ^ int n ^ "('s0)",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.PLUS, e1, T.CONST(n))))) =
            emit(A.OPER({assem="lw 'd0," ^ int n ^ "('s0)",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.MINUS, T.CONST(n), e1)))) =
            emit(A.OPER({assem="lw 'd0,-" ^ int n ^ "('s0)",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.MINUS, e1, T.CONST(n))))) =
            emit(A.OPER({assem="lw 'd0,-" ^ int n ^ "('s0)",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(e1))) =
            emit(A.OPER({assem="lw 'd0,0('s0)",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))

        (* li $1,100: load immediate value into register *)
        | munchStm (T.MOVE(T.TEMP(i), T.CONST(n))) =
            emit(A.OPER({assem="li 'd0," ^ int n,
                         src=[],
                         dst=[i],
                         jump=NONE}))

        (* move $1,$2: copy from register to register *)
        | munchStm (T.MOVE(T.TEMP(i), e1)) =
            emit(A.OPER({assem="move 'd0,'s0",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))


      and munchExp =
    in
      munchStm stm; rev(!ilist)
    end
end