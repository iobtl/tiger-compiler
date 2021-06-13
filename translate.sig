signature TRANSLATE =
sig
  type level
  type access
  type exp

  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val err : exp
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

  val simpleVar : access * level -> exp
  val fieldVar : exp * Symbol.symbol * Symbol.symbol list -> exp
  val subscriptVar : exp * Tree.exp -> exp
end