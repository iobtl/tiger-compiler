signature COLOR =
sig
  structure F : FRAME

  type allocation = MipsFrame.register Temp.Table.table

  val color : {interference: Liveness.igraph,
               initial: allocation,
               spillCost: Graph.node -> int,
               registers: MipsFrame.register list}
                -> allocation * Temp.temp list
end