signature FRAME =
sig
  type access
  type frame
  type register
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> string
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val RV : Temp.temp
  val RA : Temp.temp
  val argregs : Temp.temp list
  val wordSize : int
  val tempMap : register Temp.Table.table

  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp
  val string : Temp.label * string -> string

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
end