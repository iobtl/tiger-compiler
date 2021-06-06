structure A = Absyn

structure Semant :> SEMANT =
struct
  type ty = Ty.ty

  type tenv = ty Symbol.table
  type venv = Env.enventry Symbol.table

  fun transProg abExp =
    let
      val x = 5
    in
      print "hello"
    end

end