structure Semant :> SEMANT =
struct
  structure A = Absyn
  structure E = Env
  structure Ty = Types

  val error = ErrorMsg.error

  type tenv = E.ty Symbol.table
  type venv = E.enventry Symbol.table

  fun actual_ty ty =
    case ty of
      _ => ty

  fun checkInt ({exp, ty}, pos) =
    case ty of
      Ty.INT => ()
    | _ => error pos "integer required"

  fun checkType (ty, absty, msg, pos) =
    if ty <> absty
    then ((error pos msg); Ty.INT)
    else absty

  fun transProg abExp =
    let
      val x = 5
    in
      print "hello"
    end

  fun transExp(venv, tenv) =
    (* Arithmetic operations *)
    let fun trexp (A.OpExp({left, oper=A.PlusOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.MinusOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.TimesOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.DivideOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.LtOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.LeOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.GtOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.GeOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.EqOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.OpExp({left, oper=A.NeqOp, right, pos})) =
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=(), ty=Ty.INT})
          | trexp (A.VarExp(v)) =
                  trvar v
          | trexp (A.NilExp) =
                  {exp=(), ty=Ty.NIL}
          | trexp (A.IntExp(i)) =
                  {exp=(), ty=Ty.INT}
          | trexp (A.StringExp(s, pos)) =
                  {exp=(), ty=Ty.STRING}
          (*| trexp (A.CallExp({f, args, pos})) =*)

          (*| trexp (A.RecordExp({fields, typ, pos})) =*)
          | trexp (A.SeqExp(expls)) =
                  let
                    val res = List.map trexp (List.map (fn (exp, _) => exp) expls)
                  in
                    case res of
                      [] => {exp=(), ty=Ty.NIL}
                    | _ => List.last res
                  end
          | trexp (A.AssignExp({var, exp, pos})) =
                  let
                    val vres = trvar var
                    val eres = trexp exp
                  in
                    {exp=(), ty=Ty.UNIT}
                  end
          | trexp (A.IfExp({test, then', else', pos})) =
                  let
                    val {exp=testexp, ty=testty} = trexp test
                    val {exp=thenexp, ty=thenty} = trexp then'
                  in
                    checkType(Ty.INT, testty, "invalid test condition type", pos);
                    case else' of
                      NONE => (checkType(Ty.UNIT, thenty, "invalid expression type in if-then statement", pos); 
                              {exp=(), ty=Ty.INT})
                    | SOME(e) => (let
                                    val {exp=elseexp, ty=elsety} = trexp e
                                  in
                                    (checkType(thenty, elsety, "mismatched types in then-else branches", pos);
                                    {exp=(), ty=thenty})
                                  end)
                  end
          | trexp (A.WhileExp({test, body, pos})) =
                  let
                    val {exp=testexp, ty=testty} = trexp test
                    val {exp=bodyexp, ty=bodyty} = trexp body
                  in
                    checkType(Ty.INT, testty, "invalid test condition type", pos);
                    {exp=(), ty=bodyty}
                  end
          | trexp (A.ForExp({var, escape, lo, hi, body, pos})) =
                  let
                    val {exp=loexp, ty=loty} = trexp lo
                    val {exp=hiexp, ty=hity} = trexp hi
                    val {exp=bodyexp, ty=bodyty} = trexp body
                  in
                    checkType(loty, hity, "mismatched bounds types in for loop", pos);
                    {exp=(), ty=bodyty}
                  end
          | trexp (A.BreakExp(pos)) = (* need to check if within for/while statement *)
                  {exp=(), ty=Ty.UNIT}
          | trexp (A.LetExp({decs, body, pos})) =
                  let
                    val (venv', tenv') = List.foldl 
                                         (fn (x, (venv, tenv)) => transDec (venv, tenv, x)) 
                                         (venv, tenv) decs
                  val newTrexp = transExp(venv', tenv')
                  in
                    newTrexp body
                  end
          (*| trexp (A.ArrayExp({typ, size, init, pos})) =*)

        and trvar (A.SimpleVar(sym, pos)) =
                  case Symbol.look(venv, sym) of
                      SOME(E.VarEntry({ty})) => {exp=(), ty=actual_ty ty}
                    | NONE => (error pos ("undefined variable " ^ (Symbol.name sym));
                              {exp=(), ty=Ty.INT})
    in
      trexp
    end
end