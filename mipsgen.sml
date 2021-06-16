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
            emit(A.OPER({assem="sw 's1,0('s0)\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))

        (* lw $1,c($2): copy from memory to register *)
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.PLUS, T.CONST(n), e1)))) =
            emit(A.OPER({assem="lw 'd0," ^ int n ^ "('s0)\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.PLUS, e1, T.CONST(n))))) =
            emit(A.OPER({assem="lw 'd0," ^ int n ^ "('s0)\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.MINUS, T.CONST(n), e1)))) =
            emit(A.OPER({assem="lw 'd0,-" ^ int n ^ "('s0)\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(T.BINOP(T.MINUS, e1, T.CONST(n))))) =
            emit(A.OPER({assem="lw 'd0,-" ^ int n ^ "('s0)\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
        | munchStm (T.MOVE(T.TEMP(i), T.MEM(e1))) =
            emit(A.OPER({assem="lw 'd0,0('s0)\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))

        (* li $1,100: load immediate value into register *)
        | munchStm (T.MOVE(T.TEMP(i), T.CONST(n))) =
            emit(A.OPER({assem="li 'd0," ^ int n ^ "\n",
                         src=[],
                         dst=[i],
                         jump=NONE}))

        (* move $1,$2: copy from register to register *)
        | munchStm (T.MOVE(T.TEMP(i), e1)) =
            emit(A.OPER({assem="move 'd0,'s0\n",
                         src=[munchExp e1],
                         dst=[i],
                         jump=NONE}))
      
      and result gen = let val t = Temp.newtemp() in gen t; t end

        (** ARITHMETIC INSTRUCTIONS **)
        (* add $1,$2,$3 *)
      and munchExp (T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER({assem="add 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* sub $1,$2,$3 *)
        | munchExp (T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER({assem="sub 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* addi $1,$2,100: add immediate value *)
        | munchExp (T.BINOP(T.PLUS, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="addi 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.PLUS, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="addi 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* mul $1,$2,$3 *)
        | munchExp (T.BINOP(T.MUL, e1, e2)) =
            result(fn r => emit(A.OPER({assem="mul 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* TODO: div $2,$3 *)
        | munchExp (T.BINOP(T.DIV, e1, e2)) =
            result(fn r => emit(A.OPER({assem="div 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (** LOGICAL INSTRUCTIONS **)
    in
      munchStm stm; rev(!ilist)
    end
end