(* Straight-line program interpreter *)

type id = string

type table = (id * int) list

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

exception IdentifierNotFound

fun max a b =
    if a > b
    then a
    else b

fun maxargs stmt =
    let
        fun read_exp e =
            case e of
                OpExp(e1, _, e2) => max (read_exp e1) (read_exp e2)
              | EseqExp(s, e) => max (maxargs s) (read_exp e)
              | _ => 0
    in
        case stmt of
            CompoundStm(s1, s2) => max (maxargs s1) (maxargs s2)
          | AssignStm(_, e) => read_exp e
          | PrintStm(el) => List.length el
    end

fun interp stmt =
    let
        fun lookup table id =
            case table of
                [] => raise IdentifierNotFound
              | (s1, s2)::xs => if s1 = id then s2 else lookup xs id

        fun interpExp (exp, table) =
            case exp of
                IdExp(id) => ((lookup table id), table)
              | NumExp(num) => (num, table)
              | OpExp(e1, binop, e2) => 
                      let
                          val (res1, table2) = interpExp (e1, table)
                          val (res2, table3) = interpExp (e2, table2)
                      in
                          case binop of
                              Plus => (res1 + res2, table3)
                            | Minus => (res1 - res2, table3)
                            | Times => (res1 * res2, table3)
                            | Div => (res1 div res2, table3)
                      end
              | EseqExp(s, e) => interpExp (e, (interpStm (s, table)))

        and interpStm (stmt, table) =
            case stmt of
                CompoundStm(s1, s2) => interpStm (s2, (interpStm (s1, table)))
              | AssignStm(id, e) =>
                      let
                          val (res, table2) = interpExp (e, table)
                    in
                          (id, res)::table2
                    end
              | PrintStm(el) => table (* do nothing? *)

    in
        interpStm (stmt, [])
    end


val prog = CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b", EseqExp(PrintStm([IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)]),
        OpExp(NumExp 10, Times, IdExp "a"))),
    PrintStm([IdExp "b"])))