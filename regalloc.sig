signature REG_ALLOC =
sig	
  structure F : FRAME

  type allocation = F.register Temp.Table.table

  val alloc : Assem.instr list * F.frame ->
                        Assem.instr list * allocation
end