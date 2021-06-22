signature STACK =
sig
  type 'a stack = 'a list
  exception Empty of string
  val new : unit -> 'a stack
  val push : 'a stack * 'a -> 'a stack
  val pop : 'a stack -> 'a stack * 'a
  val isEmpty : 'a stack -> bool
end
