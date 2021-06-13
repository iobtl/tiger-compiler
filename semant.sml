structure Semant :> SEMANT =
struct
  structure A = Absyn
  structure E = Env
  structure Ty = Types
  structure T = Translate

  val error = ErrorMsg.error

  type tenv = E.ty Symbol.table
  type venv = E.enventry Symbol.table

  fun actual_ty ty =
    case ty of
      Ty.NAME(sym, ref(NONE)) => Ty.NIL
    | Ty.NAME(sym, ref(SOME(t))) => actual_ty t
    | Ty.ARRAY(t, u) => Ty.ARRAY(actual_ty t, u)
    (*| Ty.RECORD(fieldls, u) => Ty.RECORD(List.map (fn (s, t) => (s, actual_ty t)) fieldls, u)*)
    | _ => ty

  fun checkInt ({exp, ty}, pos) =
    case ty of
      Ty.INT => ()
    | _ => error pos "integer required"

  fun checkType (ty, absty, msg, pos) =
    let
      val t1 = actual_ty ty
      val t2 = actual_ty absty
    in
      if t1 <> t2
      then 
        case (t1, t2) of
          (Ty.RECORD(_, _), Ty.NIL) => ()
        | (Ty.NIL, Ty.RECORD(_, _)) => ()
        | _ => error pos msg
      else ()
  end

  fun printType ty =
    print (case ty of
      Ty.RECORD(_, _) => "record"
    | Ty.NIL => "nil"
    | Ty.INT => "int"
    | Ty.STRING => "string"
    | Ty.ARRAY(_, _) => "array" 
    | Ty.NAME(_, _) => "name"
    | Ty.UNIT => "unit")

  fun transProg abExp =
    let
      val outer_level = T.newLevel {parent=T.outermost, 
                                    name=Temp.namedlabel "main", 
                                    formals=[]}
      val translator = transExp(E.base_venv, E.base_tenv, outer_level)
    in
      (translator abExp; ())
    end

  and transExp(venv, tenv, level, break) =
    let fun trexp (A.OpExp({left, oper, right, pos})) =
                  let
                    val leftex = trexp left
                    val rightex = trexp right
                  in
                    checkInt(leftex);
                    checkInt(rightex);
                    {exp=T.opExp(left, oper, right), ty=Ty.INT}
                  end
          | trexp (A.VarExp(v)) =
                  trvar v
          | trexp (A.NilExp) =
                  {exp=T.nilExp, ty=Ty.NIL}
          | trexp (A.IntExp(i)) =
                  {exp=T.intExp(i), ty=Ty.INT}
          | trexp (A.StringExp(s, pos)) =
                  {exp=(), ty=Ty.STRING}
          | trexp (A.CallExp({func, args, pos})) =
                  let
                    val argtyps = case List.map trexp args of
                      [] => []
                    | xs => List.map #ty xs
                    val {level=call_level, formals=formtyps, result=result} = 
                      case Symbol.look(venv, func) of
                        SOME(E.FunEntry({level, label, formals, result})) => {level=level, formals=formals, result=result}
                      | SOME(E.VarEntry(_)) => (error pos "expected function but found variable"; 
                                               {level=T.Top, formals=[], result=Ty.NIL})
                      | NONE => (error pos "function not found in current environment"; 
                                {level=T.Top, formals=[], result=Ty.NIL})
                  in
                    case ListPair.allEq (fn (a, b) => a = b) (argtyps, formtyps) of
                      false => (error pos ("invalid argument type passed to function " ^ (Symbol.name func));
                               {exp=T.err, ty=Ty.NIL})
                    | true => {exp=T.callExp(func, args, level, call_level), ty=actual_ty result}
                  end
          | trexp (A.RecordExp({fields, typ, pos})) =
                  let
                    fun check_record (recls, fieldls, pos) =
                      if (List.length recls) <> (List.length fieldls)
                      then error pos (Int.toString(List.length recls) ^ " defined fields, but only " 
                           ^ Int.toString(List.length fieldls) ^ " fields provided in expression")
                      else
                        List.app (fn ((_, a), b) => checkType(a, b, "mismatched record field types", pos))
                        (ListPair.zip (recls, fieldls))

                    val record_ty = case Symbol.look (tenv, typ) of
                      NONE => (error pos ("undefined record type " ^ (Symbol.name typ)); Ty.NIL)
                    | SOME(ty) => (print ("processed record " ^ (Symbol.name typ) ^ "\n"); ty)
                  in
                    case actual_ty record_ty of
                      Ty.RECORD(rectyps, u) =>
                        let
                          val fieldls = List.map (fn (_, exp, _) => trexp exp) fields
                          val fieldtyps = List.map (fn ({exp, ty}) => ty) fieldls
                        in
                          check_record(rectyps, fieldtyps, pos); {exp=T.recordExp fields, ty=Ty.RECORD(rectyps, u)}
                        end
                      | _ => (error pos ("undefined record type " ^ (Symbol.name typ)); {exp=T.err, ty=Ty.NIL})
                  end
          | trexp (A.SeqExp(expls)) =
                  let
                    val res = List.map trexp (List.map (fn (exp, _) => exp) expls)
                  in
                    case res of
                      [] => {exp=T.nilExp, ty=Ty.UNIT}
                    | _ => List.last res
                  end
          | trexp (A.AssignExp({var, exp, pos})) =
                  let
                    val {exp=varres, ty=_} = trvar var
                    val {exp=expres, ty=_} = trexp exp
                  in
                    {exp=T.assignExp(varres, expres), ty=Ty.UNIT}
                  end
          | trexp (A.IfExp({test, then', else', pos})) =
                  let
                    val {exp=testexp, ty=testty} = trexp test
                    val {exp=thenexp, ty=thenty} = trexp then'
                  in
                    checkType(Ty.INT, testty, "invalid test condition type", pos);
                    case else' of
                      NONE => (checkType(Ty.UNIT, thenty, "invalid expression type in if-then statement", pos); 
                              {exp=T.ifThenExp(test, then'), ty=Ty.INT})
                    | SOME(e) => (let
                                    val {exp=elseexp, ty=elsety} = trexp e
                                  in
                                    (checkType(thenty, elsety, "mismatched types in then-else branches", pos);
                                    {exp=T.ifThenElseExp(test, then', elseexp), ty=thenty})
                                  end)
                  end
          | trexp (A.WhileExp({test, body, pos})) =
                  let
                    val {exp=testexp, ty=testty} = trexp test
                    val {exp=bodyexp, ty=bodyty} = trexp body
                  in
                    checkType(Ty.INT, testty, "invalid test condition type", pos);
                    {exp=T.whileExp(testexp, bodyexp, break), ty=bodyty}
                  end
          | trexp (A.ForExp({var, escape, lo, hi, body, pos})) =
                  let
                    val {exp=loexp, ty=loty} = trexp lo
                    val {exp=hiexp, ty=hity} = trexp hi
                    val {exp=bodyexp, ty=bodyty} = trexp body
                  in
                    checkType(loty, hity, "mismatched bounds types in for loop", pos);
                    {exp=T.forExp(loexp, hiexp, bodyexp, break), ty=bodyty}
                  end
          | trexp (A.BreakExp(_)) = (* need to check if within for/while statement *)
                  {exp=T.breakExp(break), ty=Ty.UNIT}
          | trexp (A.LetExp({decs, body, pos})) =
                  let
                    val {venv=venv', tenv=tenv'} = List.foldl 
                                         (fn (x, {venv, tenv}) => transDec (venv, tenv, level, x)) 
                                         {venv=venv, tenv=tenv} decs
                    val newTrexp = transExp(venv', tenv', level)
                  in
                    newTrexp body
                  end
          | trexp (A.ArrayExp({typ, size, init, pos})) =
                  let
                    val array_ty = case Symbol.look(tenv, typ) of
                      NONE => (error pos ("undefined array type " ^ (Symbol.name typ)); Ty.NIL)
                    | SOME(ty) => ty

                    val {exp=sizeexp, ty=sizety} = trexp size
                    val {exp=initexp, ty=initty} = trexp init
                  in
                    case actual_ty array_ty of
                      Ty.ARRAY(aty, u) =>
                        (checkType(Ty.INT, sizety, "invalid array size type", pos);
                        checkType(aty, initty, "mismatching array initialization type", pos);
                        {exp=T.arrayExp(sizeexp, initexp), ty=Ty.ARRAY(aty, u)})
                    | _ => (error pos ("undefined array type " ^ (Symbol.name typ)); 
                           {exp=T.err, ty=Ty.NIL})
                  end
        and trvar (A.SimpleVar(sym, pos)) =
                  (case Symbol.look(venv, sym) of
                      SOME(E.VarEntry({access, ty})) => {exp=T.simpleVar(access, level),
                                                         ty=actual_ty ty}
                    | SOME(E.FunEntry(_)) => (error pos ("expected variable but got function " ^
                                             (Symbol.name sym)); {exp=T.err, ty=Ty.NIL})
                    | NONE => (error pos ("undefined variable " ^ (Symbol.name sym));
                              {exp=T.err, ty=Ty.NIL}))
          | trvar (A.FieldVar(var, sym, pos)) =
                  let
                    val {exp=varres, ty=vty} = trvar var
                  in
                    case actual_ty vty of
                      Ty.RECORD(fieldls, u) =>
                        if List.exists (fn (x, _) => x = sym) fieldls
                        then {exp=T.fieldVar(varres, sym, fieldls), ty=actual_ty vty}
                        else (error pos ("record field " ^ (Symbol.name sym) ^ " not found");
                             {exp=T.err, ty=Ty.NIL})
                    | _ => (error pos "variable not of record type"; {exp=T.err, ty=Ty.NIL})
                  end
          | trvar (A.SubscriptVar(var, subexp, pos)) =
                  let
                    val {exp=subres, ty=subty} = trexp subexp
                    val {exp=varres, ty=vty} = trvar var
                  in
                    case actual_ty vty of
                      Ty.ARRAY(ty, u) =>
                        (checkType(Ty.INT, subty, "subscript expression not of type int", pos);
                        {exp=T.subscriptVar(varres, subres), ty=actual_ty ty})
                    | _ => (error pos "variable not of array type"; {exp=T.err, ty=Ty.NIL})
                  end
    in
      trexp
    end

  and transDec(venv, tenv, level, A.VarDec({name, escape, typ=NONE, init, pos})) =
              let
                val {exp, ty} = transExp(venv, tenv, level) init
                val access = T.allocLocal level (!escape)
              in
                {tenv=tenv, 
                 venv=Symbol.enter(venv, name, E.VarEntry({access=access, ty=ty})),
                 exps=[exp]}
              end

    | transDec(venv, tenv, level, A.VarDec({name, escape, typ=SOME(sym, sympos), init, pos})) =
              let
                val {exp=initexp, ty=initty} = transExp(venv, tenv, level) init
                val {exp=typexp, ty=typty} =
                  case Symbol.look(tenv, sym) of
                    NONE => (error pos ("variable initialization type " ^ 
                                        (Symbol.name sym) ^ " not found");
                                        {exp=(), ty=Ty.NIL})
                  | SOME(ty) => {exp=(), ty=actual_ty ty}
                val access = T.allocLocal level (!escape)
              in
                checkType(initty, typty, "mismatching declaration type", pos);
                {tenv=tenv, 
                 venv=Symbol.enter(venv, name, E.VarEntry({access=access, ty=typty})),
                 exps=[initexp]}
              end

    | transDec(venv, tenv, level, A.TypeDec(decs)) =
              let
                val tenv' = List.foldl (fn ({name, ...}, env) =>
                                       Symbol.enter(env, name, Ty.NAME(name, ref NONE)))
                                       tenv decs
                val tenv'' = List.foldl (fn ({name, ty, ...}, env) =>
                                        case Symbol.look(env, name) of
                                          SOME(Ty.NAME(n, r)) =>
                                            (r := SOME(transTy(env, ty)); env))
                                        tenv' decs
              in
                {venv=venv, tenv=tenv'', exps=[]}
              end

    | transDec(venv, tenv, level, A.FunctionDec(decs)) =
              let
                fun get_param_ty {name, typ, pos, escape} =
                  case Symbol.look(tenv, typ) of
                    NONE => ((error pos "invalid function parameter type"); Ty.INT)
                  | SOME(t) => t

                (* insert function skeleton headers first to allow for mutually 
                recursive declarations *)
                fun transheaders ({name, params, result, body, pos} : A.fundec, env) =
                  let
                    fun get_escape (params : A.field list)  =
                      List.map (fn x => (!x)) (List.map #escape params)

                    val return_ty = case result of
                      NONE => Ty.UNIT
                    | SOME(s, p) => (case Symbol.look(tenv, s) of
                                      NONE => (error pos ("invalid function result type " ^ (Symbol.name s));
                                              Ty.INT)
                                    | SOME(t) => t)

                    val param_tys = List.map get_param_ty params
                    val label = Temp.newlabel()
                    val new_level = T.newLevel {parent=level, 
                                                name=label, 
                                                formals=get_escape params}
                  in
                    Symbol.enter(env, name, E.FunEntry({level=new_level, 
                                                        label=label,
                                                        formals=param_tys, 
                                                        result=return_ty}))
                  end

                fun transbody ({name, params, result, body, pos} : A.fundec, {tenv, venv}) =
                  let
                    val venv' = List.foldl transheaders venv decs
                    val SOME(E.FunEntry({level, label, formals, result})) = Symbol.look(venv', name)

                    fun transparam ({name, escape, typ, pos}) =
                      case Symbol.look(tenv, typ) of
                        NONE => (error pos ("invalid function parameter type " ^ (Symbol.name typ)); 
                                {name=name, ty=Ty.INT, escape=escape})
                      | SOME(t) => {name=name, ty=t, escape=escape}

                    (* evaluate body expression with processed parameter list and
                     previously (header-) augmented environment *)
                    val params' = List.map transparam params
                    val venv'' = List.foldl (fn ({name, ty, escape}, env) => 
                                Symbol.enter(env, name, E.VarEntry({access=(T.allocLocal level (!escape)),
                                                                    ty=ty})))
                                venv' params'
                    val {exp, ty} = transExp(venv'', tenv, level) body
                  in
                    {venv=venv', tenv=tenv, exps=[]}
                  end
              in
                List.foldl transbody {venv=venv, tenv=tenv} decs
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
              | SOME(ty) => Ty.ARRAY(ty, ref()))
    
end