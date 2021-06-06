structure S = Symbol
structure Ty = Types

structure Env :> ENV =
struct
  type ty = Ty.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result : ty}

  val std_fun = [
    ("print", [Ty.STRING], Ty.UNIT),
    ("flush", [Ty.UNIT], Ty.UNIT),
    ("getchar", [Ty.UNIT], Ty.STRING),
    ("ord", [Ty.STRING], Ty.INT),
    ("chr", [Ty.INT], Ty.STRING),
    ("size", [Ty.STRING], Ty.INT),
    ("substring", [Ty.STRING, Ty.INT, Ty.INT], Ty.STRING),
    ("concat", [Ty.STRING, Ty.STRING], Ty.STRING),
    ("not", [Ty.INT], Ty.INT),
    ("exit", [Ty.INT], Ty.UNIT)
  ]

  val base_tenv =
    S.enter(S.enter(S.empty, (S.symbol "int"), Ty.INT), (S.symbol "string"), Ty.STRING)

  val base_venv =
    let
      fun create_symbol (s, _, _) = S.symbol s
      fun create_fun_entry (_, formals, result) = FunEntry({formals=formals, result=result})

      fun init_std_fun fun_ls =
        List.foldl 
          (fn (f, acc) => S.enter(acc, create_symbol f, create_fun_entry f))
          S.empty fun_ls
    in
      init_std_fun std_fun
    end
end