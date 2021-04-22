functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)(*-------------- SECTION 1 - Right now its empty-----------------*)


(*#line 14.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\017\000\002\000\016\000\003\000\015\000\005\000\014\000\
\\014\000\013\000\017\000\012\000\018\000\011\000\019\000\010\000\
\\020\000\009\000\044\000\008\000\048\000\007\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\184\000\011\000\184\000\013\000\184\000\015\000\184\000\
\\016\000\184\000\022\000\184\000\023\000\184\000\024\000\184\000\
\\025\000\184\000\027\000\184\000\036\000\184\000\037\000\184\000\
\\038\000\184\000\039\000\184\000\042\000\184\000\045\000\184\000\
\\046\000\184\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\185\000\011\000\185\000\013\000\185\000\015\000\185\000\
\\016\000\185\000\022\000\185\000\023\000\185\000\024\000\185\000\
\\025\000\185\000\027\000\185\000\036\000\185\000\037\000\185\000\
\\038\000\185\000\039\000\185\000\042\000\185\000\045\000\185\000\
\\046\000\185\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\186\000\011\000\186\000\013\000\186\000\015\000\186\000\
\\016\000\186\000\022\000\186\000\023\000\186\000\024\000\186\000\
\\025\000\186\000\027\000\186\000\036\000\186\000\037\000\186\000\
\\038\000\186\000\039\000\186\000\042\000\186\000\045\000\186\000\
\\046\000\186\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\187\000\011\000\187\000\013\000\187\000\015\000\187\000\
\\016\000\187\000\022\000\187\000\023\000\187\000\024\000\187\000\
\\025\000\187\000\027\000\187\000\036\000\187\000\037\000\187\000\
\\038\000\187\000\039\000\187\000\042\000\187\000\045\000\187\000\
\\046\000\187\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\188\000\011\000\188\000\013\000\188\000\015\000\188\000\
\\016\000\188\000\022\000\188\000\023\000\188\000\024\000\188\000\
\\025\000\188\000\027\000\188\000\036\000\188\000\037\000\188\000\
\\038\000\188\000\039\000\188\000\042\000\188\000\045\000\188\000\
\\046\000\188\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\008\000\189\000\011\000\189\000\013\000\189\000\015\000\189\000\
\\016\000\189\000\022\000\189\000\023\000\189\000\024\000\189\000\
\\025\000\189\000\027\000\189\000\036\000\189\000\037\000\189\000\
\\038\000\189\000\039\000\189\000\042\000\189\000\045\000\189\000\
\\046\000\189\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\013\000\079\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\013\000\084\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\022\000\076\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\
\\045\000\107\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\
\\046\000\077\000\000\000\
\\001\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\
\\046\000\127\000\000\000\
\\001\000\008\000\000\000\000\000\
\\001\000\010\000\102\000\021\000\101\000\048\000\100\000\000\000\
\\001\000\011\000\085\000\000\000\
\\001\000\011\000\122\000\000\000\
\\001\000\014\000\091\000\000\000\
\\001\000\015\000\078\000\000\000\
\\001\000\015\000\081\000\000\000\
\\001\000\015\000\116\000\000\000\
\\001\000\024\000\098\000\000\000\
\\001\000\026\000\090\000\035\000\089\000\000\000\
\\001\000\026\000\117\000\000\000\
\\001\000\026\000\125\000\029\000\124\000\000\000\
\\001\000\026\000\139\000\000\000\
\\001\000\029\000\086\000\000\000\
\\001\000\029\000\088\000\000\000\
\\001\000\029\000\128\000\000\000\
\\001\000\029\000\135\000\000\000\
\\001\000\035\000\075\000\000\000\
\\001\000\035\000\115\000\000\000\
\\001\000\042\000\070\000\000\000\
\\001\000\043\000\113\000\000\000\
\\001\000\048\000\044\000\000\000\
\\001\000\048\000\050\000\000\000\
\\001\000\048\000\072\000\000\000\
\\001\000\048\000\073\000\000\000\
\\001\000\048\000\074\000\000\000\
\\001\000\048\000\104\000\000\000\
\\001\000\048\000\120\000\000\000\
\\001\000\048\000\121\000\000\000\
\\001\000\048\000\126\000\000\000\
\\001\000\048\000\130\000\000\000\
\\001\000\048\000\136\000\000\000\
\\001\000\048\000\140\000\000\000\
\\143\000\000\000\
\\144\000\001\000\017\000\002\000\016\000\003\000\015\000\005\000\014\000\
\\014\000\013\000\017\000\012\000\018\000\011\000\019\000\010\000\
\\020\000\009\000\044\000\008\000\048\000\007\000\000\000\
\\145\000\000\000\
\\146\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\016\000\030\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\152\000\012\000\020\000\028\000\019\000\035\000\018\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\159\000\000\000\
\\160\000\023\000\108\000\000\000\
\\161\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\162\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\025\000\043\000\038\000\042\000\039\000\041\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\169\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\170\000\000\000\
\\171\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\172\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\036\000\023\000\037\000\022\000\000\000\
\\173\000\048\000\106\000\000\000\
\\174\000\000\000\
\\175\000\027\000\132\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\006\000\032\000\007\000\031\000\000\000\
\\181\000\006\000\032\000\007\000\031\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\190\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\000\000\
\\191\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\029\000\029\000\030\000\028\000\031\000\027\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\000\000\
\\192\000\048\000\069\000\000\000\
\\193\000\000\000\
\\194\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\027\000\112\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\195\000\000\000\
\\196\000\001\000\017\000\002\000\016\000\003\000\015\000\005\000\014\000\
\\014\000\013\000\017\000\012\000\018\000\011\000\019\000\010\000\
\\020\000\009\000\044\000\008\000\048\000\007\000\000\000\
\\197\000\000\000\
\\198\000\004\000\034\000\005\000\033\000\006\000\032\000\007\000\031\000\
\\027\000\083\000\029\000\029\000\030\000\028\000\031\000\027\000\
\\032\000\026\000\033\000\025\000\034\000\024\000\036\000\023\000\
\\037\000\022\000\000\000\
\\199\000\000\000\
\\200\000\010\000\037\000\012\000\036\000\014\000\035\000\000\000\
\\201\000\000\000\
\\202\000\043\000\096\000\000\000\
\\203\000\000\000\
\"
val actionRowNumbers =
"\047\000\059\000\055\000\046\000\
\\049\000\097\000\066\000\068\000\
\\034\000\000\000\000\000\047\000\
\\000\000\053\000\052\000\051\000\
\\000\000\035\000\000\000\048\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\093\000\000\000\089\000\
\\073\000\032\000\068\000\036\000\
\\037\000\038\000\030\000\009\000\
\\011\000\018\000\058\000\061\000\
\\098\000\007\000\087\000\088\000\
\\005\000\006\000\003\000\004\000\
\\002\000\001\000\049\000\086\000\
\\085\000\084\000\083\000\019\000\
\\095\000\008\000\015\000\026\000\
\\047\000\069\000\027\000\022\000\
\\017\000\000\000\000\000\000\000\
\\060\000\100\000\050\000\057\000\
\\094\000\000\000\099\000\056\000\
\\000\000\021\000\014\000\000\000\
\\039\000\076\000\010\000\063\000\
\\064\000\095\000\000\000\091\000\
\\067\000\070\000\080\000\033\000\
\\076\000\071\000\031\000\020\000\
\\023\000\000\000\000\000\096\000\
\\054\000\090\000\040\000\041\000\
\\016\000\000\000\024\000\042\000\
\\012\000\062\000\028\000\082\000\
\\081\000\072\000\000\000\043\000\
\\078\000\000\000\000\000\074\000\
\\029\000\077\000\044\000\065\000\
\\091\000\000\000\025\000\092\000\
\\075\000\045\000\078\000\079\000\
\\013\000"
val gotoT =
"\
\\001\000\140\000\003\000\004\000\008\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\002\000\038\000\007\000\037\000\013\000\036\000\000\000\
\\000\000\
\\003\000\043\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\044\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\004\000\008\000\045\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\046\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\047\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\003\000\049\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\003\000\050\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\051\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\052\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\053\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\054\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\055\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\056\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\057\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\058\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\059\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\060\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\061\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\062\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\064\000\005\000\063\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\065\000\014\000\002\000\015\000\001\000\000\000\
\\004\000\066\000\000\000\
\\000\000\
\\000\000\
\\002\000\038\000\007\000\069\000\013\000\036\000\000\000\
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
\\000\000\
\\009\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\008\000\085\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\090\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\091\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\092\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\093\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\095\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\012\000\097\000\000\000\
\\003\000\101\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\010\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\107\000\000\000\
\\003\000\108\000\014\000\002\000\015\000\001\000\000\000\
\\016\000\109\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\112\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\116\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\117\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\121\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\127\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\011\000\129\000\000\000\
\\003\000\131\000\014\000\002\000\015\000\001\000\000\000\
\\003\000\132\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\135\000\000\000\
\\003\000\136\000\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\139\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 141
val numrules = 61
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
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
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
datatype svalue = VOID | ntVOID of unit | ID of  (string) | CONST_STR of  (string) | CONST_INT of  (int) | FIELD2 of  ( ( Tiger.id * Tiger.exp )  list) | ARITH of  (Tiger.exp) | LVALUE of  (Tiger.lvalue) | FUNCDEF of  (Tiger.FuncDec) | TY of  (Tiger.ty) | TYFIELDS2 of  ( ( Tiger.id * Tiger.type_id  )  list) | TYFIELDS of  (Tiger.tyfields) | EXPS2 of  (Tiger.exp list) | EXPS of  (Tiger.exp list) | DECS of  (Tiger.dec list) | PARAM2 of  (Tiger.exp list) | PARAM of  (Tiger.exp list) | FIELD of  ( ( Tiger.id * Tiger.exp )  list) | EXP of  (Tiger.exp) | DEC of  (Tiger.dec) | PROGRAM of  (Tiger.exp list)
end
type svalue = MlyValue.svalue
type result = Tiger.exp list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 42))::
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "CONST_INT"
  | (T 1) => "CONST_STR"
  | (T 2) => "NIL"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "MUL"
  | (T 6) => "DIVIDE"
  | (T 7) => "EOF"
  | (T 8) => "NEWLINE"
  | (T 9) => "LBRAC_C"
  | (T 10) => "RBRAC_C"
  | (T 11) => "LBRAC_SQ"
  | (T 12) => "RBRAC_SQ"
  | (T 13) => "LBRAC_R"
  | (T 14) => "RBRAC_R"
  | (T 15) => "SEMICOLON"
  | (T 16) => "WHILE"
  | (T 17) => "IF"
  | (T 18) => "FOR"
  | (T 19) => "LET"
  | (T 20) => "ARRAY"
  | (T 21) => "THEN"
  | (T 22) => "ELSE"
  | (T 23) => "END"
  | (T 24) => "FUNCTION"
  | (T 25) => "COLON"
  | (T 26) => "COMMA"
  | (T 27) => "DOT"
  | (T 28) => "EQ"
  | (T 29) => "NEQ"
  | (T 30) => "LT"
  | (T 31) => "GT"
  | (T 32) => "GTE"
  | (T 33) => "LTE"
  | (T 34) => "ASSIGN"
  | (T 35) => "OR"
  | (T 36) => "AND"
  | (T 37) => "VAR"
  | (T 38) => "TYPE"
  | (T 39) => "DUB_Q"
  | (T 40) => "SIN_Q"
  | (T 41) => "IN"
  | (T 42) => "OF"
  | (T 43) => "BREAK"
  | (T 44) => "TO"
  | (T 45) => "DO"
  | (T 46) => "EXP_MINUS"
  | (T 47) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXPS EXPS, EXPS1left, EXPS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 116.35 "tiger.grm"*)EXPS(*#line 534.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXPS1left, EXPS1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 123.36 "tiger.grm"*) []           (*#line 538.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXPS2 EXPS2, _, EXPS21right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 124.36 "tiger.grm"*) EXP :: EXPS2 (*#line 542.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, EXP1left, EXPS21right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.EXPS2 ((*#line 125.36 "tiger.grm"*) []           (*#line 546.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXPS2 EXPS2, _, EXPS21right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.EXPS2 ((*#line 126.36 "tiger.grm"*) EXP :: EXPS2 (*#line 550.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, SEMICOLON1left, EXPS21right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.CONST_INT CONST_INT, CONST_INT1left, CONST_INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 131.46 "tiger.grm"*) Tiger.IExpr CONST_INT                    (*#line 554.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, CONST_INT1left, CONST_INT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONST_STR CONST_STR, CONST_STR1left, CONST_STR1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 132.46 "tiger.grm"*) Tiger.SExpr CONST_STR                    (*#line 558.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, CONST_STR1left, CONST_STR1right), rest671)
end
|  ( 7, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 133.46 "tiger.grm"*) Tiger.NilExpr                            (*#line 562.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, NIL1left, NIL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 134.46 "tiger.grm"*) Tiger.ConvArrAST (Tiger.Sub ID) EXP1 EXP2(*#line 566.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 135.46 "tiger.grm"*) Tiger.LValueExpr LVALUE                  (*#line 570.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRAC_C1right)) :: ( _, ( MlyValue.FIELD FIELD, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 136.46 "tiger.grm"*) Tiger.ConvRecAST (Tiger.Sub ID) FIELD    (*#line 574.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, RBRAC_C1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RBRAC_R1right)) :: ( _, ( MlyValue.PARAM PARAM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 137.46 "tiger.grm"*) Tiger.ConvFunCallAST ID PARAM            (*#line 578.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, RBRAC_R1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 139.46 "tiger.grm"*) Tiger.NegationExpr EXP                   (*#line 582.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, MINUS1left, EXP1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ARITH ARITH, ARITH1left, ARITH1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 140.46 "tiger.grm"*) ARITH                                    (*#line 586.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ARITH1left, ARITH1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RBRAC_R1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: ( _, ( _, LBRAC_R1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 141.46 "tiger.grm"*) Tiger.SeqExpr EXPS                       (*#line 590.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LBRAC_R1left, RBRAC_R1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 142.46 "tiger.grm"*) Tiger.AssignExpr (LVALUE,EXP)            (*#line 594.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 143.46 "tiger.grm"*) Tiger.ConvIfElseAST EXP1 EXP2 EXP3       (*#line 598.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP3right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 144.46 "tiger.grm"*) Tiger.ConvIfAST EXP1 EXP2                (*#line 602.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 145.46 "tiger.grm"*) Tiger.ConvWhileAST EXP1 EXP2             (*#line 606.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 146.46 "tiger.grm"*) Tiger.ConvForAST ID EXP1 EXP2 EXP3       (*#line 610.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, FOR1left, EXP3right), rest671)
end
|  ( 20, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 147.46 "tiger.grm"*) Tiger.BreakExpr                          (*#line 614.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 21, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 148.46 "tiger.grm"*) Tiger.ConvLetAST DECS EXPS               (*#line 618.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 22, ( rest671)) => let val  result = MlyValue.DECS ((*#line 157.43 "tiger.grm"*) []           (*#line 622.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 158.43 "tiger.grm"*) DEC :: DECS  (*#line 626.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, DEC1left, DECS1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.TY TY, _, TY1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 160.43 "tiger.grm"*) Tiger.TypeDec (ID,TY)                    (*#line 630.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, TYPE1left, TY1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 161.43 "tiger.grm"*) Tiger.ConvVarDec ID EXP                  (*#line 634.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, VAR1left, EXP1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 162.43 "tiger.grm"*) Tiger.ConvVarDecTy ID1 (Tiger.TC ID2) EXP(*#line 638.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, VAR1left, EXP1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.FUNCDEF FUNCDEF, FUNCDEF1left, FUNCDEF1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 163.43 "tiger.grm"*) Tiger.FuncDecs FUNCDEF                   (*#line 642.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, FUNCDEF1left, FUNCDEF1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.FUNCDEF ((*#line 166.67 "tiger.grm"*) Tiger.ConvFunDecAST ID TYFIELDS EXP                      (*#line 646.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.FUNCDEF ((*#line 167.67 "tiger.grm"*) Tiger.ConvFunDecRetAST ID1 TYFIELDS (Tiger.Sub ID2) EXP  (*#line 650.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 30, ( rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 177.46 "tiger.grm"*) Tiger.TF([])                                  (*#line 654.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 31, ( ( _, ( MlyValue.TYFIELDS2 TYFIELDS2, _, TYFIELDS21right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 178.46 "tiger.grm"*) Tiger.TF((ID1 , (Tiger.TC ID2)) :: TYFIELDS2) (*#line 658.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, TYFIELDS21right), rest671)
end
|  ( 32, ( rest671)) => let val  result = MlyValue.TYFIELDS2 ((*#line 179.46 "tiger.grm"*) []                                  (*#line 662.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 33, ( ( _, ( MlyValue.TYFIELDS2 TYFIELDS2, _, TYFIELDS21right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDS2 ((*#line 180.46 "tiger.grm"*) (ID1 , (Tiger.TC ID2)) :: TYFIELDS2 (*#line 666.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMMA1left, TYFIELDS21right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.TY ((*#line 183.46 "tiger.grm"*) Tiger.AlreadyTy (Tiger.TC ID) (*#line 670.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RBRAC_C1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: ( _, ( _, LBRAC_C1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 184.46 "tiger.grm"*) Tiger.SeqTy TYFIELDS          (*#line 674.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LBRAC_C1left, RBRAC_C1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( _, ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 185.46 "tiger.grm"*) Tiger.ArrayTy (Tiger.TC ID)   (*#line 678.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 191.38 "tiger.grm"*) Tiger.plus EXP1 EXP2           (*#line 682.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 192.38 "tiger.grm"*) Tiger.minus EXP1 EXP2          (*#line 686.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 193.38 "tiger.grm"*) Tiger.mul EXP1 EXP2            (*#line 690.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 194.38 "tiger.grm"*) Tiger.divv EXP1 EXP2           (*#line 694.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 195.38 "tiger.grm"*) Tiger.equals EXP1 EXP2         (*#line 698.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 196.38 "tiger.grm"*) Tiger.angbrac EXP1 EXP2        (*#line 702.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 197.38 "tiger.grm"*) Tiger.greaterThan EXP1 EXP2    (*#line 706.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 198.38 "tiger.grm"*) Tiger.lessThan EXP1 EXP2       (*#line 710.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 199.38 "tiger.grm"*) Tiger.lessEqualThan EXP1 EXP2  (*#line 714.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 200.38 "tiger.grm"*) Tiger.greatEqualThan EXP1 EXP2 (*#line 718.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 201.38 "tiger.grm"*) Tiger.andd EXP1 EXP2           (*#line 722.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARITH ((*#line 202.38 "tiger.grm"*) Tiger.orr EXP1 EXP2            (*#line 726.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, EXP1left, EXP2right), rest671)
end
|  ( 49, ( rest671)) => let val  result = MlyValue.FIELD ((*#line 210.36 "tiger.grm"*) []                   (*#line 730.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 50, ( ( _, ( MlyValue.FIELD2 FIELD2, _, FIELD21right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.FIELD ((*#line 211.36 "tiger.grm"*) (ID , EXP) :: FIELD2 (*#line 734.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, ID1left, FIELD21right), rest671)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.FIELD2 ((*#line 212.36 "tiger.grm"*) []                   (*#line 738.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.FIELD2 FIELD2, _, FIELD21right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.FIELD2 ((*#line 213.36 "tiger.grm"*) (ID , EXP) :: FIELD2 (*#line 742.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, COMMA1left, FIELD21right), rest671)
end
|  ( 53, ( rest671)) => let val  result = MlyValue.PARAM ((*#line 222.38 "tiger.grm"*) []            (*#line 746.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 54, ( ( _, ( MlyValue.PARAM2 PARAM2, _, PARAM21right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.PARAM ((*#line 223.38 "tiger.grm"*) EXP :: PARAM2 (*#line 750.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, EXP1left, PARAM21right), rest671)
end
|  ( 55, ( rest671)) => let val  result = MlyValue.PARAM2 ((*#line 224.38 "tiger.grm"*) []            (*#line 754.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 56, ( ( _, ( MlyValue.PARAM2 PARAM2, _, PARAM21right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.PARAM2 ((*#line 225.38 "tiger.grm"*) EXP :: PARAM2 (*#line 758.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, COMMA1left, PARAM21right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 228.42 "tiger.grm"*) Tiger.SimpleVar ID                       (*#line 762.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 229.42 "tiger.grm"*) Tiger.FeildVar (LVALUE , ID)             (*#line 766.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RBRAC_SQ1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 230.42 "tiger.grm"*) Tiger.ArrEl ((Tiger.SimpleVar ID) , EXP) (*#line 770.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, RBRAC_SQ1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RBRAC_SQ1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 231.42 "tiger.grm"*) Tiger.ArrEl (LVALUE , EXP)               (*#line 774.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, LVALUE1left, RBRAC_SQ1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST_INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST_INT i,p1,p2))
fun CONST_STR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.CONST_STR i,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun LBRAC_C (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun RBRAC_C (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun LBRAC_SQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun RBRAC_SQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LBRAC_R (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun RBRAC_R (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun DUB_Q (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun SIN_Q (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun EXP_MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.ID i,p1,p2))
end
end
