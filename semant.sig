signature SEMANT =
sig
  val transProg : Absyn.exp -> Translate.frag list
  val transExp : Env.enventry Symbol.table * Env.ty Symbol.table * Translate.level * Temp.label
                -> Absyn.exp -> {exp: Translate.exp, ty: Types.ty}
end