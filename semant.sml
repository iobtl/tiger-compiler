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
          | trexp (A.CallExp({func, args, pos})) =
                  let
                    val argtyps = case List.map trexp args of
                      [] => []
                    | xs => List.map #ty xs
                    val {formals=formtyps, result=result} = case Symbol.look(venv, func) of
                      NONE => (error pos "function not found in current environment"; {formals=[], result=Ty.INT})
                    | SOME(E.FunEntry({formals, result})) => {formals=formals, result=result}
                  in
                    case ListPair.allEq (fn (a, b) => a = b) (argtyps, formtyps) of
                      false => (error pos ("invalid argument type passed to function " ^ (Symbol.name func));
                               {exp=(), ty=Ty.INT})
                    | true => {exp=(), ty=result}
                  end
          | trexp (A.RecordExp({fields, typ, pos})) =
                  let
                    fun check_record (recls, fieldls, pos) =
                      if (List.length recls) <> (List.length fieldls)
                      then error pos (Int.toString(List.length recls) ^ " defined fields, but only " 
                           ^ Int.toString(List.length fieldls) ^ " fields provided in expression")
                      else
                        (List.map (fn ((_, a), b) => checkType(a, b, "mismatched record field types", pos))
                        (ListPair.zip (recls, fieldls)); ())

                    val record_ty = case Symbol.look (tenv, typ) of
                      NONE => (error pos ("undefined record type " ^ (Symbol.name typ)); Ty.INT)
                    | SOME(ty) => ty
                  in
                    case record_ty of
                      Ty.RECORD(rectyps, u) =>
                        let
                          val fieldls = List.map (fn (_, exp, _) => trexp exp) fields
                          val fieldtyps = List.map (fn ({exp, ty}) => ty) fieldls
                        in
                          check_record(rectyps, fieldtyps, pos); {exp=(), ty=Ty.RECORD(rectyps, u)}
                        end
                      | _ => (error pos ("undefined record type " ^ (Symbol.name typ)); {exp=(), ty=Ty.INT})
                  end
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
                    val {venv=venv', tenv=tenv'} = List.foldl 
                                         (fn (x, {venv, tenv}) => transDec (venv, tenv, x)) 
                                         {venv=venv, tenv=tenv} decs
                  val newTrexp = transExp(venv', tenv')
                  in
                    newTrexp body
                  end
          | trexp (A.ArrayExp({typ, size, init, pos})) =
                  let
                    val array_ty = case Symbol.look(tenv, typ) of
                      NONE => (error pos ("undefined array type " ^ (Symbol.name typ)); Ty.INT)
                    | SOME(ty) => ty

                    val {exp=_, ty=sizety} = trexp size
                    val {exp=_, ty=initty} = trexp init
                  in
                    case array_ty of
                      Ty.ARRAY(aty, u) =>
                        (checkType(Ty.INT, sizety, "invalid array size type", pos);
                        checkType(aty, initty, "mismatching array initialization type", pos);
                        {exp=(), ty=Ty.ARRAY(aty, u)})
                    | _ => (error pos ("undefined array type " ^ (Symbol.name typ)); {exp=(), ty=Ty.INT})
                  end
        and trvar (A.SimpleVar(sym, pos)) =
                  (case Symbol.look(venv, sym) of
                      SOME(E.VarEntry({ty})) => {exp=(), ty=actual_ty ty}
                    | NONE => (error pos ("undefined variable " ^ (Symbol.name sym));
                              {exp=(), ty=Ty.INT}))
          | trvar (A.FieldVar(var, sym, pos)) =
                  let
                    val {exp=_, ty=vty} = trvar var
                  in
                    case vty of
                      Ty.RECORD(fieldls, u) =>
                        if List.exists (fn (x, _) => x = sym) fieldls
                        then {exp=(), ty=vty}
                        else (error pos ("record field " ^ (Symbol.name sym) ^ " not found");
                             {exp=(), ty=Ty.INT})
                    | _ => (error pos "variable not of record type"; {exp=(), ty=Ty.INT})
                  end
          | trvar (A.SubscriptVar(var, subexp, pos)) =
                  let
                    val {exp=_, ty=subty} = trexp subexp
                    val {exp=_, ty=vty} = trvar var
                  in
                    case vty of
                      Ty.ARRAY(ty, u) =>
                        (checkType(Ty.INT, subty, "subscript expression not of type int", pos);
                        {exp=(), ty=ty})
                    | _ => (error pos "variable not of array type"; {exp=(), ty=Ty.INT})
                  end
    in
      trexp
    end

  and transDec(venv, tenv, A.VarDec({name, escape, typ=NONE, init, pos})) =
              let
                val {exp, ty} = transExp(venv, tenv) init
              in
                {tenv=tenv, venv=Symbol.enter(venv, name, E.VarEntry({ty=ty}))}
              end

    | transDec(venv, tenv, A.VarDec({name, escape, typ=SOME(sym, sympos), init, pos})) =
              let
                val {exp=initexp, ty=initty} = transExp(venv, tenv) init
                val {exp=typexp, ty=typty} =
                  case Symbol.look(tenv, sym) of
                    NONE => {exp=(), ty=Ty.NIL}
                  | SOME(ty) => {exp=(), ty=ty}
              in
                checkType(initty, typty, "mismatching declaration type", pos);
                {tenv=tenv, venv=Symbol.enter(venv, name, E.VarEntry({ty=initty}))}
              end

    | transDec(venv, tenv, A.TypeDec(decs)) =
              let
                fun add_type {name, ty, pos} tenv' = Symbol.enter(tenv', name, transTy(tenv', ty))
              in
                {venv=venv,
                 tenv=(List.foldl (fn (dec, tenv') => add_type dec tenv') tenv decs)}
              end

    | transDec(venv, tenv, A.FunctionDec({name, params, body, pos, result}::decs)) =
              let
                fun transparam {name, escape, typ, pos} =
                  case Symbol.look(tenv, typ) of
                    NONE => ((error pos "invalid parameter type"); {name=name, ty=Ty.INT})
                  | SOME(t) => {name=name, ty=t}
                fun enterparam ({name, ty}, venv) =
                  Symbol.enter(venv, name, E.VarEntry({ty=ty}))

                val result_ty = case result of
                  NONE => Ty.UNIT
                | SOME(rt, pos) => (case Symbol.look(tenv, rt) of
                                    NONE => ((error pos "invalid result type"); Ty.INT)
                                  | SOME(t) => t)
                val params' = List.map transparam params
                val venv' = Symbol.enter(venv, name, E.FunEntry({formals=(List.map #ty params'),
                                                                 result=result_ty}))
                val venv'' = List.foldl enterparam venv' params'
              in
                transExp(venv'', tenv) body; {venv=venv', tenv=tenv}
              end

  and transTy(tenv, A.NameTy(sym, pos)) =
              (case Symbol.look(tenv, sym) of
                NONE => (error pos ("invalid type used in type declaration: " ^ (Symbol.name sym));
                          Ty.INT)
              | SOME(ty) => ty)

    | transTy(tenv, A.RecordTy(fieldls)) =
              let
                fun get_type {name, escape, typ, pos} =
                case Symbol.look(tenv, typ) of 
                  NONE => (error pos ("invalid type used in type declaration: " ^ (Symbol.name typ));
                            Ty.INT)
                | SOME(ty) => ty

                val fields = ListPair.zip ((List.map (fn x => #name x) fieldls), (List.map get_type fieldls))
              in
                Ty.RECORD(fields, ref())
              end

    | transTy(tenv, A.ArrayTy(sym, pos)) =
              (case Symbol.look(tenv, sym) of
                NONE => (error pos ("invalid type used in type declaration: " ^ (Symbol.name sym));
                          Ty.INT)
              | SOME(ty) => ty)
    
end