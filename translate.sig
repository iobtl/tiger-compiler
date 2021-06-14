signature TRANSLATE =
sig
  type level
  type access
  type exp
  type frag

  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val err : exp
  val nilexp : exp
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

  val intExp : int -> exp
  val stringExp : string -> exp

  val simpleVar : access * level -> exp
  val fieldVar : exp * Symbol.symbol * Symbol.symbol list -> exp
  val subscriptVar : exp * exp -> exp

  val opExp : exp * Absyn.oper * exp -> exp
  val assignExp : exp * exp -> exp
  val ifThenElseExp : exp * exp * exp -> exp
  val ifThenExp : exp * exp -> exp
  val callExp : Temp.label * exp list * level * level * bool -> exp
  val whileExp : exp * exp * Temp.label -> exp
  val forExp : exp * exp * exp * Temp.label -> exp
  val recordExp : exp list -> exp
  val breakExp : Temp.label -> exp
  val letExp : exp list * exp -> exp
  val arrayExp : exp * exp -> exp

  val procEntryExit : {level: level, body: exp} -> unit

  structure F : FRAME
  val getResult : unit -> frag list
end