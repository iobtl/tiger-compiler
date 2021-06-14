signature FRAME =
sig
  type access
  type frame
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val RV : Temp.temp
  val wordSize: int

  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
end