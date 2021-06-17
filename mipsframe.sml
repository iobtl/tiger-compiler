structure MipsFrame : FRAME =
struct
  structure T = Tree
  structure A = Assem

  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = {formals: access list, instructs: int,
                locals: int ref, name: Temp.label}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  type register = string

  (** Special Registers **)

  (* zero register *)
  val ZERO = Temp.newtemp()

  (* reserved for pseudo-instructions *)
  val at = Temp.newtemp()

  (* return values from functions *)
  val v0 = Temp.newtemp()
  val v1 = Temp.newtemp()

  (* (caller-saved) arguments to functions - not preserved by subprograms *)
  val a0 = Temp.newtemp()
  val a1 = Temp.newtemp()
  val a2 = Temp.newtemp()
  val a3 = Temp.newtemp()

  (* temporary data - not preserved by subprograms *)
  val t0 = Temp.newtemp()
  val t1 = Temp.newtemp()
  val t2 = Temp.newtemp()
  val t3 = Temp.newtemp()
  val t4 = Temp.newtemp()
  val t5 = Temp.newtemp()
  val t6 = Temp.newtemp()
  val t7 = Temp.newtemp()

  (* (callee-saved) saved registers - preserved by subprograms *)
  val s0 = Temp.newtemp()
  val s1 = Temp.newtemp()
  val s2 = Temp.newtemp()
  val s3 = Temp.newtemp()
  val s4 = Temp.newtemp()
  val s5 = Temp.newtemp()
  val s6 = Temp.newtemp()
  val s7 = Temp.newtemp()

  (* temporary registers - not preserved by subprograms *)
  val t8 = Temp.newtemp()
  val t9 = Temp.newtemp()

  (* designated pointers/address values *)
  val GP = Temp.newtemp()
  val SP = Temp.newtemp()
  val FP = Temp.newtemp()
  val RA = Temp.newtemp()
  val RV = Temp.newtemp()

  val specialregs = [GP, SP, FP, RA, RV, ZERO]
  val argregs = [a0, a1, a2, a3]
  val calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]
  val tempregs = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]

  val wordSize = 4

  val temp_mapping = 
    [(GP, "gp"),
     (SP, "sp"),
     (FP, "fp"),
     (RA, "ra"),
     (RV, "rv"),
     (ZERO, "zero")]

  val tempMap = List.foldl (fn ((reg, name), tab) => Temp.Table.enter(tab, reg, name)) 
                Temp.Table.empty temp_mapping

  fun newFrame {name, formals} =
    let
      val offset = ref 0

      (* static link and incoming arguments are written in 'previous frame' *)
      fun get_access true = (offset := (!offset) + wordSize; InFrame((!offset)))
        | get_access false = InReg(Temp.newtemp())

      val formals_access = List.map get_access formals
    in
      {formals=formals_access, instructs=0,
       locals=(ref 0), name=name}
    end

  fun name (frame : frame) = Symbol.name (#name frame)
  fun formals (frame : frame) = #formals frame
  fun allocLocal (frame : frame) esc =
    let
      val offset = (!(#locals frame)) * ~wordSize 
    in
      (#locals frame) := (!(#locals frame)) + 1;
      case esc of
        true => InFrame(offset)
      | false => InReg(Temp.newtemp())
    end

  fun exp (InFrame(k)) addr = T.MEM(T.BINOP(T.PLUS, addr, T.CONST(k)))    
    | exp (InReg(t)) _ = T.TEMP(t)

  fun externalCall (s, args) =
    T.CALL(T.NAME(Temp.namedlabel(s)), args)

  fun string (label, str) =
    (Symbol.name label) ^  ": .asciiz \"" ^ str ^ "\"\n"

  (* view shift *)
  fun procEntryExit1 (frame, body) = body
  fun procEntryExit2 (frame, body) =
    body @
    [A.OPER({assem="",
             src=[ZERO, RA, SP]@calleesaves,
             dst=[],
             jump=SOME([])})]
end