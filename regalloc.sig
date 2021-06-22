signature REG_ALLOC =
sig	
  structure MipsFrame : FRAME

  type allocation = Frame.register Temp.Table.table

  val alloc : Assem.instr list * Frame.frame ->
                        Assem.instr list * allocation
end