structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int
                  | InReg of Temp.temp
  type frame = {formals: access list, instructs: int,
                locals: int ref, name: Temp.label}

  fun newFrame {name, formals} =
    let
      val offset = ref 4

      fun get_access true = (offset := (!offset) - 4; InFrame((!offset)))
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
      val offset = (!(#locals frame)) * 4
    in
      (#locals frame) := (!(#locals frame)) + 1;
      case esc of
        true => InFrame(offset)
      | false => InReg(Temp.newtemp())
    end
end