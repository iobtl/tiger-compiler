structure Stack :> STACK =
struct
  type 'a stack = 'a list
  exception Empty of string

  fun new () = []
  fun push (s, x) = x::s
  fun pop s =
    case s of 
      x::t => (t, x)
    | [] => raise Empty "empty stack"

  val isEmpty = List.null
end