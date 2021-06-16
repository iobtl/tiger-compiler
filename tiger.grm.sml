functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
type symbol = Symbol.symbol


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\195\000\005\000\195\000\007\000\195\000\009\000\195\000\
\\011\000\195\000\013\000\195\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\195\000\027\000\195\000\
\\031\000\195\000\032\000\195\000\035\000\195\000\036\000\195\000\
\\038\000\195\000\039\000\195\000\043\000\195\000\044\000\195\000\
\\045\000\195\000\000\000\
\\001\000\001\000\196\000\005\000\196\000\007\000\196\000\009\000\196\000\
\\011\000\196\000\013\000\196\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\196\000\027\000\196\000\
\\031\000\196\000\032\000\196\000\035\000\196\000\036\000\196\000\
\\038\000\196\000\039\000\196\000\043\000\196\000\044\000\196\000\
\\045\000\196\000\000\000\
\\001\000\001\000\197\000\005\000\197\000\007\000\197\000\009\000\197\000\
\\011\000\197\000\013\000\197\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\197\000\027\000\197\000\
\\031\000\197\000\032\000\197\000\035\000\197\000\036\000\197\000\
\\038\000\197\000\039\000\197\000\043\000\197\000\044\000\197\000\
\\045\000\197\000\000\000\
\\001\000\001\000\198\000\005\000\198\000\007\000\198\000\009\000\198\000\
\\011\000\198\000\013\000\198\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\198\000\027\000\198\000\
\\031\000\198\000\032\000\198\000\035\000\198\000\036\000\198\000\
\\038\000\198\000\039\000\198\000\043\000\198\000\044\000\198\000\
\\045\000\198\000\000\000\
\\001\000\001\000\199\000\005\000\199\000\007\000\199\000\009\000\199\000\
\\011\000\199\000\013\000\199\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\199\000\027\000\199\000\
\\031\000\199\000\032\000\199\000\035\000\199\000\036\000\199\000\
\\038\000\199\000\039\000\199\000\043\000\199\000\044\000\199\000\
\\045\000\199\000\000\000\
\\001\000\001\000\200\000\005\000\200\000\007\000\200\000\009\000\200\000\
\\011\000\200\000\013\000\200\000\015\000\031\000\016\000\030\000\
\\018\000\029\000\019\000\028\000\026\000\200\000\027\000\200\000\
\\031\000\200\000\032\000\200\000\035\000\200\000\036\000\200\000\
\\038\000\200\000\039\000\200\000\043\000\200\000\044\000\200\000\
\\045\000\200\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\001\000\002\000\042\000\000\000\
\\001\000\002\000\052\000\000\000\
\\001\000\002\000\070\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\102\000\012\000\101\000\029\000\100\000\000\000\
\\001\000\002\000\104\000\000\000\
\\001\000\002\000\108\000\000\000\
\\001\000\002\000\125\000\000\000\
\\001\000\002\000\131\000\000\000\
\\001\000\002\000\135\000\000\000\
\\001\000\006\000\087\000\028\000\086\000\000\000\
\\001\000\006\000\120\000\000\000\
\\001\000\006\000\129\000\020\000\128\000\000\000\
\\001\000\008\000\088\000\000\000\
\\001\000\009\000\076\000\000\000\
\\001\000\009\000\096\000\000\000\
\\001\000\009\000\118\000\000\000\
\\001\000\011\000\083\000\015\000\031\000\016\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\001\000\011\000\095\000\015\000\031\000\016\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\001\000\013\000\093\000\000\000\
\\001\000\013\000\126\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\
\\031\000\075\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\
\\035\000\109\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\
\\036\000\074\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\
\\036\000\132\000\000\000\
\\001\000\020\000\085\000\000\000\
\\001\000\020\000\094\000\000\000\
\\001\000\020\000\137\000\000\000\
\\001\000\028\000\073\000\000\000\
\\001\000\028\000\116\000\000\000\
\\001\000\038\000\069\000\000\000\
\\001\000\039\000\098\000\000\000\
\\001\000\040\000\114\000\000\000\
\\140\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\141\000\043\000\041\000\044\000\040\000\045\000\039\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\002\000\108\000\005\000\107\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\157\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\161\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\162\000\002\000\079\000\000\000\
\\163\000\005\000\123\000\015\000\031\000\016\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\164\000\000\000\
\\165\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\166\000\005\000\097\000\015\000\031\000\016\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\167\000\000\000\
\\168\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\169\000\007\000\077\000\015\000\031\000\016\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\170\000\000\000\
\\171\000\008\000\050\000\010\000\049\000\012\000\048\000\000\000\
\\172\000\040\000\112\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\176\000\010\000\019\000\014\000\018\000\028\000\017\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\186\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\187\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\
\\032\000\110\000\000\000\
\\188\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\189\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\018\000\029\000\019\000\028\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\201\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\000\000\
\\202\000\015\000\031\000\016\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\021\000\026\000\022\000\025\000\023\000\024\000\
\\024\000\023\000\025\000\022\000\026\000\021\000\000\000\
\\203\000\018\000\029\000\019\000\028\000\000\000\
\"
val actionRowNumbers =
"\007\000\086\000\078\000\042\000\
\\079\000\092\000\043\000\008\000\
\\007\000\007\000\007\000\070\000\
\\081\000\080\000\073\000\007\000\
\\009\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\047\000\060\000\
\\045\000\046\000\048\000\043\000\
\\039\000\010\000\011\000\012\000\
\\037\000\032\000\030\000\082\000\
\\023\000\071\000\064\000\007\000\
\\067\000\087\000\075\000\026\000\
\\098\000\097\000\005\000\003\000\
\\006\000\004\000\002\000\001\000\
\\095\000\096\000\094\000\099\000\
\\061\000\049\000\044\000\070\000\
\\034\000\019\000\022\000\007\000\
\\007\000\007\000\083\000\070\000\
\\028\000\035\000\027\000\024\000\
\\068\000\076\000\040\000\013\000\
\\007\000\014\000\055\000\031\000\
\\090\000\089\000\072\000\085\000\
\\007\000\074\000\084\000\067\000\
\\093\000\050\000\041\000\055\000\
\\051\000\058\000\038\000\055\000\
\\025\000\015\000\020\000\007\000\
\\007\000\065\000\007\000\069\000\
\\016\000\029\000\007\000\056\000\
\\021\000\055\000\017\000\033\000\
\\088\000\064\000\077\000\053\000\
\\052\000\059\000\007\000\018\000\
\\057\000\054\000\007\000\066\000\
\\062\000\036\000\091\000\007\000\
\\063\000\000\000"
val gotoT =
"\
\\001\000\003\000\002\000\137\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\036\000\008\000\035\000\009\000\034\000\010\000\033\000\
\\014\000\032\000\015\000\031\000\016\000\030\000\000\000\
\\000\000\
\\001\000\041\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\042\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\043\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\045\000\003\000\002\000\005\000\044\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\001\000\051\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\052\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\053\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\054\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\055\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\056\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\057\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\058\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\059\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\060\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\061\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\062\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\063\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\015\000\031\000\016\000\064\000\000\000\
\\000\000\
\\000\000\
\\009\000\034\000\010\000\065\000\000\000\
\\007\000\066\000\008\000\035\000\009\000\034\000\010\000\033\000\
\\014\000\032\000\015\000\031\000\016\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\076\000\000\000\
\\001\000\078\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\080\000\003\000\002\000\004\000\079\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\045\000\003\000\002\000\005\000\082\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\087\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\088\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\089\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\001\000\045\000\003\000\002\000\005\000\090\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\097\000\000\000\
\\001\000\101\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\012\000\104\000\013\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\109\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\080\000\003\000\002\000\004\000\111\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\113\000\013\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\115\000\013\000\103\000\000\000\
\\000\000\
\\013\000\117\000\000\000\
\\000\000\
\\001\000\119\000\003\000\002\000\017\000\001\000\000\000\
\\001\000\120\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\001\000\122\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\125\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\012\000\128\000\013\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\131\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\132\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\134\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\136\000\003\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 138
val numrules = 64
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | array_init of unit ->  (A.exp)
 | func_declaration_ls of unit ->  (A.fundec list)
 | func_declaration of unit ->  (A.fundec)
 | var_declaration of unit ->  (A.dec)
 | ty_field of unit ->  (A.field)
 | ty_fields of unit ->  (A.field list) | ty of unit ->  (A.ty)
 | ty_declaration_ls of unit ->  ({ name:symbol,ty:A.ty,pos:pos }  list)
 | ty_declaration of unit ->  ({ name:symbol,ty:A.ty,pos:pos } )
 | declaration of unit ->  (A.dec)
 | declaration_list of unit ->  (A.dec list)
 | field_list of unit ->  ( ( symbol * A.exp * pos )  list)
 | exp_seq of unit ->  ( ( A.exp * pos )  list)
 | exps of unit ->  (A.exp list) | lval of unit ->  (A.var)
 | program of unit ->  (A.exp) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | (T 45) => "FUNDEC"
  | (T 46) => "TYDEC"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40)
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.declaration_list (fn
 _ => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.declaration_list declaration_list1, _, 
declaration_list1right)) :: ( _, ( MlyValue.declaration declaration1, 
declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.declaration_list (fn _ => let val  (declaration as 
declaration1) = declaration1 ()
 val  (declaration_list as declaration_list1) = declaration_list1 ()
 in (declaration::declaration_list)
end)
 in ( LrTable.NT 6, ( result, declaration1left, declaration_list1right
), rest671)
end
|  ( 3, ( ( _, ( MlyValue.var_declaration var_declaration1, 
var_declaration1left, var_declaration1right)) :: rest671)) => let val 
 result = MlyValue.declaration (fn _ => let val  (var_declaration as 
var_declaration1) = var_declaration1 ()
 in (var_declaration)
end)
 in ( LrTable.NT 7, ( result, var_declaration1left, 
var_declaration1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ty_declaration_ls ty_declaration_ls1, 
ty_declaration_ls1left, ty_declaration_ls1right)) :: rest671)) => let
 val  result = MlyValue.declaration (fn _ => let val  (
ty_declaration_ls as ty_declaration_ls1) = ty_declaration_ls1 ()
 in (A.TypeDec(ty_declaration_ls))
end)
 in ( LrTable.NT 7, ( result, ty_declaration_ls1left, 
ty_declaration_ls1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.func_declaration_ls func_declaration_ls1, 
func_declaration_ls1left, func_declaration_ls1right)) :: rest671)) =>
 let val  result = MlyValue.declaration (fn _ => let val  (
func_declaration_ls as func_declaration_ls1) = func_declaration_ls1 ()
 in (A.FunctionDec(func_declaration_ls))
end)
 in ( LrTable.NT 7, ( result, func_declaration_ls1left, 
func_declaration_ls1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ty_declaration ty_declaration1, 
ty_declaration1left, ty_declaration1right)) :: rest671)) => let val  
result = MlyValue.ty_declaration_ls (fn _ => let val  (ty_declaration
 as ty_declaration1) = ty_declaration1 ()
 in ([ty_declaration])
end)
 in ( LrTable.NT 9, ( result, ty_declaration1left, 
ty_declaration1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ty_declaration_ls ty_declaration_ls1, _, 
ty_declaration_ls1right)) :: ( _, ( MlyValue.ty_declaration 
ty_declaration1, ty_declaration1left, _)) :: rest671)) => let val  
result = MlyValue.ty_declaration_ls (fn _ => let val  (ty_declaration
 as ty_declaration1) = ty_declaration1 ()
 val  (ty_declaration_ls as ty_declaration_ls1) = ty_declaration_ls1
 ()
 in (ty_declaration::ty_declaration_ls)
end)
 in ( LrTable.NT 9, ( result, ty_declaration1left, 
ty_declaration_ls1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.ty_declaration (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({name=(Symbol.symbol ID), ty=ty, pos=TYPEleft})
end)
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(Symbol.symbol ID, IDleft))
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ty_fields 
ty_fields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (ty_fields as ty_fields1)
 = ty_fields1 ()
 in (A.RecordTy(ty_fields))
end)
 in ( LrTable.NT 10, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(Symbol.symbol ID, ARRAYleft))
end)
 in ( LrTable.NT 10, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ty_field (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
{name=(Symbol.symbol ID1), escape=ref true, typ=(Symbol.symbol ID2), pos=ID1left}
)
end)
 in ( LrTable.NT 12, ( result, ID1left, ID2right), rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.ty_fields (fn _ =>
 ([]))
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ty_fields ty_fields1, _, ty_fields1right))
 :: ( _, ( MlyValue.ty_field ty_field1, ty_field1left, _)) :: rest671)
) => let val  result = MlyValue.ty_fields (fn _ => let val  (ty_field
 as ty_field1) = ty_field1 ()
 val  (ty_fields as ty_fields1) = ty_fields1 ()
 in (ty_field::ty_fields)
end)
 in ( LrTable.NT 11, ( result, ty_field1left, ty_fields1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.ty_fields ty_fields1, _, ty_fields1right))
 :: ( _, ( MlyValue.ty_field ty_field1, _, _)) :: ( _, ( _, COMMA1left
, _)) :: rest671)) => let val  result = MlyValue.ty_fields (fn _ =>
 let val  (ty_field as ty_field1) = ty_field1 ()
 val  (ty_fields as ty_fields1) = ty_fields1 ()
 in (ty_field::ty_fields)
end)
 in ( LrTable.NT 11, ( result, COMMA1left, ty_fields1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, VAR1left, _)) :: rest671))
 => let val  result = MlyValue.var_declaration (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=(Symbol.symbol ID), escape=ref true, typ=NONE, init=exp, pos=IDleft})
)
end)
 in ( LrTable.NT 13, ( result, VAR1left, exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left,
 _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.var_declaration (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=(Symbol.symbol ID1), escape=ref true, typ=SOME(Symbol.symbol ID2, ID2left), init=exp, pos=ID1left})
)
end)
 in ( LrTable.NT 13, ( result, VAR1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.func_declaration func_declaration1, 
func_declaration1left, func_declaration1right)) :: rest671)) => let
 val  result = MlyValue.func_declaration_ls (fn _ => let val  (
func_declaration as func_declaration1) = func_declaration1 ()
 in ([func_declaration])
end)
 in ( LrTable.NT 15, ( result, func_declaration1left, 
func_declaration1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.func_declaration_ls func_declaration_ls1, _
, func_declaration_ls1right)) :: ( _, ( MlyValue.func_declaration 
func_declaration1, func_declaration1left, _)) :: rest671)) => let val 
 result = MlyValue.func_declaration_ls (fn _ => let val  (
func_declaration as func_declaration1) = func_declaration1 ()
 val  (func_declaration_ls as func_declaration_ls1) = 
func_declaration_ls1 ()
 in (func_declaration::func_declaration_ls)
end)
 in ( LrTable.NT 15, ( result, func_declaration1left, 
func_declaration_ls1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.ty_fields ty_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1
, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)
) => let val  result = MlyValue.func_declaration (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty_fields as ty_fields1) = ty_fields1 ()
 val  (exp as exp1) = exp1 ()
 in (
{name=(Symbol.symbol ID), params=ty_fields, result=NONE, body=exp, pos=FUNCTIONleft}
)
end)
 in ( LrTable.NT 14, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.ty_fields 
ty_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.func_declaration (fn _ => let val  ID1 = ID1 ()
 val  (ty_fields as ty_fields1) = ty_fields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
{name=(Symbol.symbol ID1), params=ty_fields, result=SOME(Symbol.symbol ID2, ID2left), body=exp, pos=FUNCTIONleft}
)
end)
 in ( LrTable.NT 14, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 22, ( rest671)) => let val  result = MlyValue.field_list (fn _ =>
 ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.field_list (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ([(Symbol.symbol ID, exp, IDleft)])
end)
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.field_list field_list1, _, field_list1right
)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID
 ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.field_list (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (field_list as field_list1) = field_list1 ()
 in ((Symbol.symbol ID, exp, IDleft)::field_list)
end)
 in ( LrTable.NT 5, ( result, ID1left, field_list1right), rest671)
end
|  ( 25, ( rest671)) => let val  result = MlyValue.exps (fn _ => ([]))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) =
 exp1 ()
 in ([exp])
end)
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exps exps1, _, exps1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exps as exps1) = exps1 ()
 in (exp::exps)
end)
 in ( LrTable.NT 3, ( result, exp1left, exps1right), rest671)
end
|  ( 28, ( rest671)) => let val  result = MlyValue.exp_seq (fn _ => (
[]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.exp_seq (fn _ => let val 
 (exp as exp1) = exp1 ()
 in ([(exp, expleft)])
end)
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp_seq exp_seq1, _, exp_seq1right)) :: _
 :: ( _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671))
 => let val  result = MlyValue.exp_seq (fn _ => let val  (exp as exp1)
 = exp1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in ((exp, expleft)::exp_seq)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp_seq1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lval (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.SimpleVar(Symbol.symbol ID, IDleft))
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lval (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.SubscriptVar(A.SimpleVar(Symbol.symbol ID, IDleft), exp, IDleft))

end)
 in ( LrTable.NT 2, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lval lval1, (lvalleft as lval1left), _)) :: rest671)) => let
 val  result = MlyValue.lval (fn _ => let val  (lval as lval1) = lval1
 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lval, Symbol.symbol ID, lvalleft))
end)
 in ( LrTable.NT 2, ( result, lval1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lval lval1, (lvalleft as lval1left), _))
 :: rest671)) => let val  result = MlyValue.lval (fn _ => let val  (
lval as lval1) = lval1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lval, exp, lvalleft))
end)
 in ( LrTable.NT 2, ( result, lval1left, RBRACK1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.array_init (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp({typ=(Symbol.symbol ID), size=exp1, init=exp2, pos=IDleft})
)
end)
 in ( LrTable.NT 16, ( result, ID1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.lval lval1, lval1left, lval1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lval
 as lval1) = lval1 ()
 in (A.VarExp(lval))
end)
 in ( LrTable.NT 0, ( result, lval1left, lval1right), rest671)
end
|  ( 37, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({left=A.IntExp(0), oper=A.MinusOp, right=exp, pos=MINUSleft}))

end)
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (exp_seq as exp_seq1) = 
exp_seq1 ()
 in (A.SeqExp(exp_seq))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exps exps1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  (exps as exps1) = exps1 ()
 in (A.CallExp({func=(Symbol.symbol ID), args=exps, pos=IDleft}))
end)
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.field_list 
field_list1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (field_list as field_list1) = field_list1 ()
 in (
A.RecordExp({fields=field_list, typ=(Symbol.symbol ID), pos=IDleft}))

end)
 in ( LrTable.NT 0, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.array_init array_init1, array_init1left, 
array_init1right)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (array_init as array_init1) = array_init1 ()
 in (array_init)
end)
 in ( LrTable.NT 0, ( result, array_init1left, array_init1right), 
rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lval lval1, lval1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lval
 as lval1) = lval1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp({var=lval, exp=exp, pos=ASSIGNleft}))
end)
 in ( LrTable.NT 0, ( result, lval1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.IfExp({test=exp1, then'=exp2, else'=SOME(exp3), pos=IFleft}))

end)
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp({test=exp1, then'=exp2, else'=NONE, pos=IFleft}))
end)
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp({test=exp1, body=exp2, pos=WHILEleft}))
end)
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp({var=(Symbol.symbol ID), escape=ref true, lo=exp1, hi=exp2, body=exp3, pos=FORleft})
)
end)
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 50, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 51, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: _ :: ( _, ( MlyValue.declaration_list 
declaration_list1, _, _)) :: ( _, ( _, (LETleft as LET1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
declaration_list as declaration_list1) = declaration_list1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (
A.LetExp({decs=declaration_list, body=A.SeqExp(exp_seq), pos=LETleft})
)
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.MinusOp, right=exp2, pos=MINUSleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.DivideOp, right=exp2, pos=DIVIDEleft})
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.TimesOp, right=exp2, pos=TIMESleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
EQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.EqOp, right=exp2, pos=EQleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.NeqOp, right=exp2, pos=NEQleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.GtOp, right=exp2, pos=GTleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.LtOp, right=exp2, pos=LTleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.GeOp, right=exp2, pos=GEleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.LeOp, right=exp2, pos=LEleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ANDleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1, then'=exp2, else'=SOME(A.IntExp(0)), pos=ANDleft})
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ORleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1, then'=A.IntExp(1), else'=SOME(exp2), pos=ORleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper=A.PlusOp, right=exp2, pos=PLUSleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun TYDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
end
end
