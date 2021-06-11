signature FINDESCAPE =
sig
  val findEscape: Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE =
struct
  structure A = Absyn

  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(esc: escEnv, d: depth, s: A.var) : unit =
    case s of
      A.SimpleVar(sym, pos) => 
        (case Symbol.look(esc, sym) of
          NONE => ErrorMsg.error pos "escapeError: variable not declared yet"
        | SOME(dd, br) => (if d > dd then br := true else ()))
    | A.FieldVar(var, _, _) => traverseVar(esc, d, var)
    | A.SubscriptVar(var, exp, _) => (traverseExp(esc, d, exp);
                                      traverseVar(esc, d, var))

  and traverseExp(esc: escEnv, d: depth, s: A.exp) : unit =
    case s of
      A.VarExp(v) => traverseVar(esc, d, v)
    | A.CallExp({args, ...}) => (List.map (fn x => traverseExp(esc, d, x)) args; ())
    | A.OpExp({left, right, ...}) => (traverseExp(esc, d, left);
                                      traverseExp(esc, d, right))
    | A.RecordExp({fields, ...}) => (List.map 
                                    (fn (_, e, _) => traverseExp(esc, d, e)) fields; ())
    | A.SeqExp(expls) => (List.map (fn (exp, _) => traverseExp(esc, d, exp)) expls; ())
    | A.AssignExp({var, exp, ...}) => (traverseVar(esc, d, var);
                                       traverseExp(esc, d, exp))
    | A.IfExp({test, then', else', ...}) => (traverseExp(esc, d, test);
                                           traverseExp(esc, d, then');
                                           case else' of
                                             NONE => ()
                                           | SOME(exp) => traverseExp(esc, d, exp))
    | A.WhileExp({test, body, ...}) => (traverseExp(esc, d, test);
                                      traverseExp(esc, d, body))
    | A.ForExp({var, escape, lo, hi, body, ...}) =>
      let
        val new_esc = Symbol.enter(esc, var, (d, escape))
      in
        (escape := false;
         traverseExp(new_esc, d, lo);
         traverseExp(new_esc, d, hi);
         traverseExp(new_esc, d, body))
      end
    | A.LetExp({decs, body, ...}) =>
      let
        val new_esc = traverseDecs(esc, d, decs)
      in
        traverseExp(new_esc, d, body)
      end
    | A.ArrayExp({size, init, ...}) => (traverseExp(esc, d, size);
                                      traverseExp(esc, d, init))
    | _ => ()

  and traverseDecs(env: escEnv, d: depth, s: A.dec list) : escEnv =
    let
      fun process_dec (dec, e) =
        case dec of
          A.VarDec({name, escape, init, ...}) => (escape := false;
                                                 traverseExp(e, d, init);
                                                 Symbol.enter(e, name, (d, escape)))
        | A.FunctionDec(fundecs) =>
            let
              fun process_func({params, body, ...} : A.fundec) =
                let
                  val new_env = List.foldl 
                                (fn ({name, escape, ...} : A.field, esc) => 
                                  (escape := false;
                                  Symbol.enter(esc, name, (d, escape))))
                                e params
                in
                  traverseExp(new_env, d+1, body)
                end
            in
              (List.map process_func fundecs; env)
            end
    in
      List.foldl process_dec env s
    end

  fun findEscape(prog: A.exp) : unit =
    traverseExp(Symbol.empty, 0, prog) 
end