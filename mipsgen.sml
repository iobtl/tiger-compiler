signature CODEGEN =
sig
  structure F : FRAME
  val codegen : F.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure A = Assem
  structure T = Tree
  structure F = MipsFrame

  fun codegen frame (stm: Tree.stm) : A.instr list =
    let
      val ilist = ref (nil: A.instr list)
      val calldefs = [F.RA, F.RV]@F.argregs
      fun emit x = ilist := x::(!ilist)
      fun int i = if i < 0 then Int.toString(~i) else Int.toString(i)

      fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)

        (** DATA MOVEMENT INSTRUCTIONS **)
        (* sw $1,c($2): copy from register to memory *)
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST(i))), e2)) =
            emit(A.OPER({assem="sw 's1," ^ int i ^ "('s0)\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(i), e1)), e2)) =
            emit(A.OPER({assem="sw 's1," ^ int i ^ "('s0)\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=NONE}))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST(i))), e2)) =
            emit(A.OPER({assem="sw 's1,-" ^ int i ^ "('s0)\n",
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
            emit(A.MOVE({assem="move 'd0,'s0\n",
                         src=munchExp e1,
                         dst=i}))

        (** BRANCH INSTRUCTIONS **)
        (* beqz $1,label: test if register is equal to zero *)
        | munchStm (T.CJUMP(T.EQ, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="beqz 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* beq $1,$2,label: test if registers are equal *)
        | munchStm (T.CJUMP(T.EQ, e1, e2, t, f)) =
            emit(A.OPER({assem="beq 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bnez $1, label: test if register is not equal to zero *)
        | munchStm (T.CJUMP(T.NE, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="bnez 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bne $1,$2,label: test if registers are not equal *)
        | munchStm (T.CJUMP(T.NE, e1, e2, t, f)) =
            emit(A.OPER({assem="bne 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bgtz $1,label: test if one register is greater than zero *)
        | munchStm (T.CJUMP(T.GT, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="bgtz 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bgt $1,$2,label: test if one register is greater than another register *)
        | munchStm (T.CJUMP(T.GT, e1, e2, t, f)) =
            emit(A.OPER({assem="bgt 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bgtu $1,$2,label: (unsigned) test if one register is greater than another register *)
        | munchStm (T.CJUMP(T.UGT, e1, e2, t, f)) =
            emit(A.OPER({assem="bgtu 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bgez $1,label: test if one register is greater or equal to zero *)
        | munchStm (T.CJUMP(T.GE, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="bgez 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bge $1,$2,label: test if one register is greater or equal to another register *)
        | munchStm (T.CJUMP(T.GE, e1, e2, t, f)) =
            emit(A.OPER({assem="bge 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bgeu $1,$2,label: (unsigned) test if one register is greater or equal to another register *)
        | munchStm (T.CJUMP(T.UGE, e1, e2, t, f)) =
            emit(A.OPER({assem="bgeu 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bltz $1,label: test if one register is less than zero *)
        | munchStm (T.CJUMP(T.LT, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="bltz 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* blt $1,$2,label: test if one register is less than another register *)
        | munchStm (T.CJUMP(T.LT, e1, e2, t, f)) =
            emit(A.OPER({assem="blt 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bltu $1,$2,label: (unsigned) test if one register is less than another register *)
        | munchStm (T.CJUMP(T.ULT, e1, e2, t, f)) =
            emit(A.OPER({assem="bltu 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* blez $1,label: test if one register is less or equal to zero *)
        | munchStm (T.CJUMP(T.LE, e1, T.CONST(0), t, f)) =
            emit(A.OPER({assem="blez 's0," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME([t, f])}))
        (* ble $1,$2,label: test if one register is less or equal to another register *)
        | munchStm (T.CJUMP(T.LE, e1, e2, t, f)) =
            emit(A.OPER({assem="ble 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))
        (* bleu $1,$2,label: (unsigned) test if one register is less or equal to another register *)
        | munchStm (T.CJUMP(T.ULE, e1, e2, t, f)) =
            emit(A.OPER({assem="bleu 's0,'s1," ^ (Symbol.name t) ^ "\n",
                         src=[munchExp e1, munchExp e2],
                         dst=[],
                         jump=SOME([t, f])}))

        (** JUMP INSTRUCTIONS **)
        (* j label: jump to target address *)
        | munchStm (T.JUMP(T.NAME(name), labs)) =
            emit(A.OPER({assem="j " ^ (Symbol.name name) ^ "\n",
                         src=[],
                         dst=[],
                         jump=SOME(labs)}))
        (* jr $1: jump to address stored in $1 *)
        | munchStm (T.JUMP(e1, labs)) =
            emit(A.OPER({assem="jr 's0\n",
                         src=[munchExp e1],
                         dst=[],
                         jump=SOME(labs)}))

        (* jal label: procedure call *)
        | munchStm(T.EXP(T.CALL(T.NAME(lab), args))) =
            let
              val temp_store = List.map (fn reg => (Temp.newtemp(), reg)) F.argregs
              val temp_save_instr = List.map (fn (dst, src) => T.MOVE(T.TEMP(dst), T.TEMP(src)))
                                    temp_store
              val temp_rest_instr = List.map (fn (src, dst) => T.MOVE(T.TEMP(dst), T.TEMP(src)))
                                    temp_store
            in
              List.map munchStm temp_save_instr;
              emit(A.OPER({assem="jal " ^ (Symbol.name lab) ^ "\n",
                          src=munchArgs(0, args),
                          dst=calldefs,
                          jump=SOME([lab])}));
              List.map munchStm temp_rest_instr;
              ()
            end

        | munchStm (T.LABEL(lab)) =
            emit(A.LABEL({assem=(Symbol.name lab) ^ ":\n", lab=lab}))

      (* special munch: generates instruction to move all arguments in procedure call
      to their respective argument registers; saving of registers handled by procedure-
      caller *)
      and munchArgs (idx, args) =
          let
            fun move_reg (idx, arg) =
              let
                val reg = List.nth(F.argregs, idx)
                val exp = munchExp arg
              in
                munchStm(T.MOVE(T.TEMP(reg), T.TEMP(exp)));
                reg
              end

            val ints = List.tabulate(List.length args, (fn x => x + idx))
            val lp = ListPair.zip (ints, args)
          in
            List.foldr (fn ((idx, arg), acc) => move_reg(idx,arg)::acc) [] lp
          end

      and result gen = let val t = Temp.newtemp() in gen t; t end

        (** ARITHMETIC INSTRUCTIONS **)
        (* add $1,$2,$3 *)
        (* addi $1,$2,100: add immediate value *)
      and munchExp (T.BINOP(T.PLUS, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="addi 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.PLUS, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="addi 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER({assem="add 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* sub $1,$2,c *)
        | munchExp (T.BINOP(T.MINUS, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="addi 'd0,'s0,-" ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* sub $1,$2,$3 *)
        | munchExp (T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER({assem="sub 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
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
        (* andi $1,$2,100: bitwise AND with immediate value *)
        | munchExp (T.BINOP(T.AND, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="andi 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.AND, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="andi 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* and $1,$2,$3: bitwise AND *)
        | munchExp (T.BINOP(T.AND, e1, e2)) =
            result(fn r => emit(A.OPER({assem="and 'd0,'s0',s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* ori $1,$2,100: bitwise OR with immediate value *)
        | munchExp (T.BINOP(T.OR, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="ori 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.OR, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="ori 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* or $1,$2,$3: bitwise OR *)
        | munchExp (T.BINOP(T.OR, e1, e2)) =
            result(fn r => emit(A.OPER({assem="or 'd0,'s0',s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* xori $1,$2,100: bitwise XOR with immediate value *)
        | munchExp (T.BINOP(T.XOR, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="xori 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.XOR, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="xori 'd0,'s0'," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* xor $1,$2,$3: bitwise XOR *)
        | munchExp (T.BINOP(T.XOR, e1, e2)) =
            result(fn r => emit(A.OPER({assem="xor 'd0,'s0',s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* sll $1,$2,10: shift left by constant number of bits *)
        | munchExp (T.BINOP(T.LSHIFT, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="sll 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.LSHIFT, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="sll 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* sllv $1,$2,$3: shift left by register value *)
        | munchExp (T.BINOP(T.LSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER({assem="sllv 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* srl $1,$2,10: shift right by constant number of bits; zeros shifted in *)
        | munchExp (T.BINOP(T.RSHIFT, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="srl 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.RSHIFT, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="srl 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* srlv $1,$2,$3: shift right by register value; zeros shifted in *)
        | munchExp (T.BINOP(T.RSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER({assem="srlv 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))
        (* sra $1,$2,10: shift right by constant number of bits; sign bit shifted in *)
        | munchExp (T.BINOP(T.ARSHIFT, e1, T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="sra 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.BINOP(T.ARSHIFT, T.CONST(i), e1)) =
            result(fn r => emit(A.OPER({assem="sra 'd0,'s0," ^ int i ^ "\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))
        (* srav $1,$2,$3: shift right by register value; sign bit shifted in *)
        | munchExp (T.BINOP(T.ARSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER({assem="srav 'd0,'s0,'s1\n",
                                        src=[munchExp e1, munchExp e2],
                                        dst=[r],
                                        jump=NONE})))

        (** MEMORY ACCESS INSTRUCTIONS **)
        | munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST(i)))) =
            result(fn r => emit(A.OPER({assem="lw 'd0," ^ int i ^ "\n",
                                        src=[munchExp e1], 
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST(i), e1))) =
            result(fn r => emit(A.OPER({assem="lw 'd0," ^ int i ^ "\n",
                                        src=[munchExp e1], 
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.MEM(T.BINOP(T.MINUS, e1, T.CONST(i)))) =
            result(fn r => emit(A.OPER({assem="lw 'd0,-" ^ int i ^ "\n",
                                        src=[munchExp e1], 
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.MEM(T.CONST(i))) =
            result(fn r => emit(A.OPER({assem="lw 'd0," ^ int i ^ "($zero)\n",
                                        src=[],
                                        dst=[r],
                                        jump=NONE})))
        | munchExp (T.MEM(e1)) =
            result(fn r => emit(A.OPER({assem="lw 'd0,0('s0)\n",
                                        src=[munchExp e1],
                                        dst=[r],
                                        jump=NONE})))

        | munchExp(T.CALL(T.NAME(lab), args)) =
            let
              val temp_store = List.map (fn reg => (Temp.newtemp(), reg)) F.argregs
              val temp_save_instr = List.map (fn (dst, src) => T.MOVE(T.TEMP(dst), T.TEMP(src)))
                                    temp_store
              val temp_rest_instr = List.map (fn (src, dst) => T.MOVE(T.TEMP(dst), T.TEMP(src)))
                                    temp_store
            in
              List.map munchStm temp_save_instr;
              result(fn r => emit(A.OPER({assem="jal " ^ (Symbol.name lab),
                                         src=munchArgs(0, args),
                                         dst=calldefs,
                                         jump=SOME([lab])})));
              List.map munchStm temp_rest_instr;
              F.RV
            end

        | munchExp (T.TEMP(t)) = t

        | munchExp (T.CONST(i)) =
            result(fn r => emit(A.OPER({assem="li 'd0," ^ int i ^ "\n",
                                        src=[],
                                        dst=[r],
                                        jump=NONE})))

        | munchExp (T.NAME(lab)) =
            result (fn r => emit(A.OPER({assem="la 'd0," ^ (Symbol.name lab),
                                         src=[],
                                         dst=[r],
                                         jump=NONE})))
    in
      munchStm stm; List.rev(!ilist)
    end
end