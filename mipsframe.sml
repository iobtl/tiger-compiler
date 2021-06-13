structure MipsFrame : FRAME =
struct
  structure T = Tree

  datatype access = InFrame of int
                  | InReg of Temp.temp
  type frame = {formals: access list, instructs: int,
                locals: int ref, name: Temp.label}

  val FP = Temp.newtemp()
  val wordSize = 4

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

  fun name (frame : frame) = #name frame
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
end