structure COLOR =
struct
  structure F = MipsFrame : FRAME

  type allocation = Frame.register Temp.Table.table

  val color : {interference: Liveness.igraph,
               initial: allocation,
               spillCost: Graph.node -> int,
               registers: Frame.register list}
                -> allocation * Temp.temp list

  fun color ({interference, initial, spillCost, registers}) =

end