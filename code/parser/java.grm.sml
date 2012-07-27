functor JavaLrValsFun (structure Token: TOKEN structure Ast: AST) = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Ast

local open A
in 
  structure Binop = A.Binop
  structure Tipe = A.Tipe
  structure Exp = A.Exp
  structure Stm = A.Stm
  structure Method = A.Method
  structure Class = A.Class
  structure MainClass = A.MainClass
  structure Program = A.Program
end


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\004\000\000\000\
\\001\000\003\000\026\000\000\000\
\\001\000\004\000\033\000\000\000\
\\001\000\004\000\041\000\000\000\
\\001\000\004\000\075\000\000\000\
\\001\000\004\000\085\000\023\000\084\000\026\000\083\000\027\000\082\000\
\\028\000\081\000\029\000\080\000\030\000\079\000\046\000\078\000\000\000\
\\001\000\004\000\086\000\000\000\
\\001\000\004\000\087\000\000\000\
\\001\000\004\000\144\000\000\000\
\\001\000\004\000\153\000\000\000\
\\001\000\005\000\046\000\000\000\
\\001\000\005\000\048\000\000\000\
\\001\000\005\000\123\000\006\000\111\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\005\000\146\000\006\000\111\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\005\000\147\000\006\000\111\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\005\000\148\000\006\000\111\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\005\000\155\000\000\000\
\\001\000\005\000\157\000\000\000\
\\001\000\005\000\171\000\000\000\
\\001\000\006\000\040\000\000\000\
\\001\000\006\000\074\000\033\000\073\000\000\000\
\\001\000\006\000\111\000\007\000\122\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\007\000\154\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\007\000\163\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\007\000\167\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\015\000\110\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\015\000\121\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\015\000\162\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\015\000\169\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\111\000\015\000\175\000\024\000\109\000\025\000\108\000\
\\031\000\107\000\035\000\106\000\036\000\105\000\037\000\104\000\
\\038\000\103\000\039\000\102\000\040\000\101\000\041\000\100\000\
\\042\000\099\000\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\001\000\006\000\125\000\033\000\124\000\000\000\
\\001\000\006\000\145\000\000\000\
\\001\000\007\000\032\000\000\000\
\\001\000\007\000\042\000\000\000\
\\001\000\008\000\009\000\000\000\
\\001\000\008\000\013\000\014\000\012\000\000\000\
\\001\000\008\000\022\000\000\000\
\\001\000\008\000\049\000\000\000\
\\001\000\008\000\051\000\000\000\
\\001\000\008\000\066\000\016\000\065\000\018\000\064\000\020\000\063\000\
\\029\000\062\000\034\000\061\000\046\000\071\000\000\000\
\\001\000\009\000\031\000\011\000\030\000\000\000\
\\001\000\009\000\038\000\011\000\030\000\000\000\
\\001\000\009\000\072\000\000\000\
\\001\000\009\000\090\000\000\000\
\\001\000\009\000\119\000\000\000\
\\001\000\009\000\120\000\000\000\
\\001\000\010\000\014\000\000\000\
\\001\000\011\000\011\000\000\000\
\\001\000\012\000\021\000\000\000\
\\001\000\013\000\037\000\000\000\
\\001\000\015\000\028\000\000\000\
\\001\000\015\000\091\000\000\000\
\\001\000\015\000\150\000\000\000\
\\001\000\017\000\168\000\000\000\
\\001\000\021\000\020\000\022\000\019\000\046\000\018\000\000\000\
\\001\000\021\000\114\000\046\000\113\000\000\000\
\\001\000\031\000\076\000\000\000\
\\001\000\032\000\138\000\046\000\137\000\000\000\
\\001\000\033\000\149\000\000\000\
\\001\000\033\000\170\000\000\000\
\\001\000\046\000\006\000\000\000\
\\001\000\046\000\010\000\000\000\
\\001\000\046\000\015\000\000\000\
\\001\000\046\000\023\000\000\000\
\\001\000\046\000\039\000\000\000\
\\001\000\046\000\045\000\000\000\
\\001\000\046\000\047\000\000\000\
\\001\000\046\000\069\000\000\000\
\\001\000\046\000\089\000\000\000\
\\001\000\046\000\095\000\000\000\
\\177\000\002\000\008\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\021\000\020\000\022\000\019\000\046\000\018\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\021\000\020\000\022\000\019\000\046\000\018\000\000\000\
\\192\000\019\000\054\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\006\000\025\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\199\000\006\000\074\000\033\000\073\000\000\000\
\\200\000\008\000\066\000\016\000\065\000\018\000\064\000\020\000\063\000\
\\021\000\020\000\022\000\019\000\029\000\062\000\034\000\061\000\
\\046\000\060\000\000\000\
\\200\000\008\000\066\000\016\000\065\000\018\000\064\000\020\000\063\000\
\\029\000\062\000\034\000\061\000\046\000\071\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\040\000\101\000\
\\041\000\100\000\042\000\099\000\043\000\098\000\044\000\097\000\
\\045\000\096\000\000\000\
\\212\000\006\000\111\000\024\000\109\000\031\000\107\000\035\000\106\000\
\\036\000\105\000\037\000\104\000\038\000\103\000\039\000\102\000\
\\040\000\101\000\041\000\100\000\042\000\099\000\043\000\098\000\
\\044\000\097\000\045\000\096\000\000\000\
\\213\000\006\000\111\000\031\000\107\000\037\000\104\000\038\000\103\000\
\\039\000\102\000\000\000\
\\214\000\006\000\111\000\031\000\107\000\037\000\104\000\038\000\103\000\
\\039\000\102\000\000\000\
\\215\000\006\000\111\000\031\000\107\000\000\000\
\\216\000\006\000\111\000\031\000\107\000\000\000\
\\217\000\006\000\111\000\031\000\107\000\000\000\
\\218\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\000\000\
\\219\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\000\000\
\\220\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\000\000\
\\221\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\000\000\
\\222\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\040\000\101\000\
\\041\000\100\000\042\000\099\000\043\000\098\000\000\000\
\\223\000\006\000\111\000\031\000\107\000\035\000\106\000\036\000\105\000\
\\037\000\104\000\038\000\103\000\039\000\102\000\040\000\101\000\
\\041\000\100\000\042\000\099\000\043\000\098\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\004\000\112\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\\235\000\006\000\111\000\031\000\107\000\000\000\
\\236\000\000\000\
\\237\000\004\000\085\000\023\000\084\000\026\000\083\000\027\000\082\000\
\\028\000\081\000\029\000\080\000\030\000\079\000\046\000\078\000\000\000\
\\238\000\019\000\166\000\000\000\
\\239\000\006\000\111\000\024\000\109\000\025\000\108\000\031\000\107\000\
\\035\000\106\000\036\000\105\000\037\000\104\000\038\000\103\000\
\\039\000\102\000\040\000\101\000\041\000\100\000\042\000\099\000\
\\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\\240\000\000\000\
\\241\000\006\000\111\000\024\000\109\000\025\000\108\000\031\000\107\000\
\\035\000\106\000\036\000\105\000\037\000\104\000\038\000\103\000\
\\039\000\102\000\040\000\101\000\041\000\100\000\042\000\099\000\
\\043\000\098\000\044\000\097\000\045\000\096\000\000\000\
\"
val actionRowNumbers =
"\001\000\073\000\061\000\071\000\
\\035\000\074\000\062\000\048\000\
\\036\000\047\000\063\000\077\000\
\\049\000\037\000\064\000\082\000\
\\093\000\092\000\091\000\002\000\
\\077\000\051\000\041\000\033\000\
\\003\000\082\000\077\000\083\000\
\\055\000\075\000\090\000\050\000\
\\042\000\078\000\065\000\020\000\
\\076\000\004\000\034\000\085\000\
\\066\000\011\000\067\000\012\000\
\\038\000\087\000\039\000\079\000\
\\086\000\079\000\095\000\088\000\
\\055\000\095\000\068\000\096\000\
\\043\000\080\000\094\000\005\000\
\\057\000\006\000\007\000\008\000\
\\096\000\069\000\044\000\052\000\
\\097\000\021\000\084\000\006\000\
\\006\000\006\000\070\000\026\000\
\\127\000\124\000\128\000\126\000\
\\125\000\056\000\006\000\006\000\
\\006\000\006\000\045\000\089\000\
\\046\000\081\000\027\000\022\000\
\\013\000\031\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\058\000\006\000\006\000\
\\106\000\006\000\133\000\009\000\
\\032\000\131\000\014\000\015\000\
\\016\000\098\000\072\000\103\000\
\\059\000\053\000\006\000\006\000\
\\118\000\119\000\116\000\114\000\
\\117\000\115\000\112\000\113\000\
\\111\000\110\000\109\000\010\000\
\\121\000\108\000\107\000\023\000\
\\017\000\135\000\018\000\006\000\
\\132\000\040\000\040\000\006\000\
\\101\000\028\000\024\000\133\000\
\\120\000\123\000\134\000\130\000\
\\025\000\054\000\100\000\029\000\
\\102\000\060\000\019\000\136\000\
\\006\000\129\000\040\000\105\000\
\\006\000\122\000\137\000\099\000\
\\030\000\104\000\000\000"
val gotoT =
"\
\\001\000\174\000\002\000\001\000\000\000\
\\003\000\003\000\000\000\
\\000\000\
\\004\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\015\000\012\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\025\000\012\000\014\000\000\000\
\\000\000\
\\009\000\027\000\000\000\
\\000\000\
\\000\000\
\\008\000\032\000\000\000\
\\005\000\033\000\012\000\014\000\000\000\
\\000\000\
\\012\000\034\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\042\000\016\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\048\000\000\000\
\\000\000\
\\006\000\050\000\000\000\
\\014\000\051\000\000\000\
\\006\000\053\000\000\000\
\\007\000\057\000\010\000\056\000\011\000\055\000\012\000\054\000\000\000\
\\000\000\
\\012\000\065\000\000\000\
\\007\000\057\000\010\000\066\000\011\000\055\000\012\000\054\000\000\000\
\\000\000\
\\010\000\068\000\011\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\075\000\000\000\
\\000\000\
\\000\000\
\\010\000\086\000\011\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\090\000\000\000\
\\013\000\091\000\000\000\
\\013\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\113\000\000\000\
\\013\000\114\000\000\000\
\\013\000\115\000\000\000\
\\013\000\116\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\124\000\000\000\
\\013\000\125\000\000\000\
\\013\000\126\000\000\000\
\\013\000\127\000\000\000\
\\013\000\128\000\000\000\
\\013\000\129\000\000\000\
\\013\000\130\000\000\000\
\\013\000\131\000\000\000\
\\013\000\132\000\000\000\
\\013\000\133\000\000\000\
\\013\000\134\000\000\000\
\\000\000\
\\013\000\137\000\000\000\
\\013\000\138\000\000\000\
\\000\000\
\\013\000\139\000\000\000\
\\013\000\141\000\019\000\140\000\000\000\
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
\\013\000\149\000\000\000\
\\013\000\150\000\000\000\
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
\\017\000\154\000\000\000\
\\000\000\
\\013\000\156\000\000\000\
\\000\000\
\\011\000\157\000\000\000\
\\011\000\158\000\000\000\
\\013\000\159\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\141\000\019\000\162\000\000\000\
\\000\000\
\\000\000\
\\018\000\163\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\170\000\000\000\
\\000\000\
\\011\000\171\000\000\000\
\\000\000\
\\013\000\172\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 175
val numrules = 65
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
datatype svalue = VOID' | ntVOID of unit ->  unit
 | ID of unit ->  (string) | INTEGER_LITERAL of unit ->  (int)
 | expList of unit ->  (Exp.t list) | expRest of unit ->  (Exp.t)
 | expRests of unit ->  (Exp.t list)
 | formalList of unit ->  ({ ty:Tipe.t,id:string }  list)
 | formalRests of unit ->  ({ ty:Tipe.t,id:string }  list)
 | formalRest of unit ->  ({ ty:Tipe.t,id:string } )
 | exp of unit ->  (Exp.t) | tipe of unit ->  (Tipe.t)
 | statement of unit ->  (Stm.t list)
 | statements of unit ->  (Stm.t list) | method of unit ->  (Method.t)
 | methods of unit ->  (Method.t list)
 | var of unit ->  ({ ty:Tipe.t,id:string } )
 | vars of unit ->  ({ ty:Tipe.t,id:string }  list)
 | cvars of unit ->  ({ ty:Tipe.t,id:string,prefix:string }  list)
 | class of unit ->  (Class.t) | classes of unit ->  (Class.t list)
 | mainClass of unit ->  (MainClass.t) | prog of unit ->  (Program.t)
end
type svalue = MlyValue.svalue
type result = Program.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "CLASS"
  | (T 2) => "MAIN"
  | (T 3) => "LPAREN"
  | (T 4) => "RPAREN"
  | (T 5) => "LBRACK"
  | (T 6) => "RBRACK"
  | (T 7) => "LBRACE"
  | (T 8) => "RBRACE"
  | (T 9) => "STATIC"
  | (T 10) => "PUBLIC"
  | (T 11) => "VOID"
  | (T 12) => "STRING"
  | (T 13) => "EXTENDS"
  | (T 14) => "SEMICOLON"
  | (T 15) => "WHILE"
  | (T 16) => "ELSE"
  | (T 17) => "IF"
  | (T 18) => "COMMA"
  | (T 19) => "RETURN"
  | (T 20) => "INT"
  | (T 21) => "BOOLEAN"
  | (T 22) => "NOT"
  | (T 23) => "AND"
  | (T 24) => "OR"
  | (T 25) => "NEW"
  | (T 26) => "TRUE"
  | (T 27) => "FALSE"
  | (T 28) => "THIS"
  | (T 29) => "INTEGER_LITERAL"
  | (T 30) => "DOT"
  | (T 31) => "LENGTH"
  | (T 32) => "ASSIGN"
  | (T 33) => "SYSTEM_OUT_PRINTLN"
  | (T 34) => "PLUS"
  | (T 35) => "MINUS"
  | (T 36) => "TIMES"
  | (T 37) => "DIV"
  | (T 38) => "MOD"
  | (T 39) => "GT"
  | (T 40) => "GE"
  | (T 41) => "LT"
  | (T 42) => "LE"
  | (T 43) => "EQ"
  | (T 44) => "NEQ"
  | (T 45) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 45) => MlyValue.ID(fn () => ("bogus")) | 
_ => MlyValue.VOID'
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.classes classes1, _, classes1right)) :: ( _
, ( MlyValue.mainClass mainClass1, (mainClassleft as mainClass1left),
 _)) :: rest671)) => let val  result = MlyValue.prog (fn _ => let val 
 (mainClass as mainClass1) = mainClass1 ()
 val  (classes as classes1) = classes1 ()
 in (
Program.T {mainClass = mainClass,
							         classes = classes,
							         pos = mainClassleft}
)
end)
 in ( LrTable.NT 0, ( result, mainClass1left, classes1right), rest671)

end
|  ( 1, ( ( _, ( _, _, RBRACE2right)) :: _ :: ( _, ( 
MlyValue.statements statements1, _, _)) :: ( _, ( MlyValue.vars vars1,
 _, _)) :: _ :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: _ ::
 _ :: _ :: _ :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, 
( _, (CLASSleft as CLASS1left), _)) :: rest671)) => let val  result = 
MlyValue.mainClass (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (vars as vars1) = vars1 ()
 val  (statements as statements1) = statements1 ()
 in (
MainClass.T {name = ID1,
								  arg = ID2,
								  locals = vars,
								  stms = statements,
								  pos = CLASSleft}
)
end)
 in ( LrTable.NT 1, ( result, CLASS1left, RBRACE2right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.classes (fn _ => ([]
))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.class class1, _, class1right)) :: ( _, ( 
MlyValue.classes classes1, classes1left, _)) :: rest671)) => let val  
result = MlyValue.classes (fn _ => let val  (classes as classes1) = 
classes1 ()
 val  (class as class1) = class1 ()
 in (classes @ [class])
end)
 in ( LrTable.NT 2, ( result, classes1left, class1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.methods 
methods1, _, _)) :: ( _, ( MlyValue.cvars cvars1, _, _)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, (CLASSleft as CLASS1left), _))
 :: rest671)) => let val  result = MlyValue.class (fn _ => let val  (
ID as ID1) = ID1 ()
 val  (cvars as cvars1) = cvars1 ()
 val  (methods as methods1) = methods1 ()
 in (
 Class.T {name = ID,
						  extends = NONE,
						  cvars = cvars,
						  methods = methods,
						  pos = CLASSleft}
)
end)
 in ( LrTable.NT 3, ( result, CLASS1left, RBRACE1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.methods 
methods1, _, _)) :: ( _, ( MlyValue.cvars cvars1, _, _)) :: _ :: ( _, 
( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, (CLASSleft as CLASS1left), _)) :: rest671)) => let val  
result = MlyValue.class (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (cvars as cvars1) = cvars1 ()
 val  (methods as methods1) = methods1 ()
 in (
 Class.T {name = ID1,
					      extends = SOME ID2,
					      cvars = cvars,
					      methods = methods,
					      pos = CLASSleft}
)
end)
 in ( LrTable.NT 3, ( result, CLASS1left, RBRACE1right), rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.cvars (fn _ => ([]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( MlyValue.cvars cvars1, _, cvars1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.tipe tipe1, tipe1left, _)
) :: rest671)) => let val  result = MlyValue.cvars (fn _ => let val  (
tipe as tipe1) = tipe1 ()
 val  (ID as ID1) = ID1 ()
 val  (cvars as cvars1) = cvars1 ()
 in (
{ty = tipe,
                 id = ID,
                 prefix = ""} :: cvars 
)
end)
 in ( LrTable.NT 4, ( result, tipe1left, cvars1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.vars (fn _ => ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.var var1, _, var1right)) :: ( _, ( 
MlyValue.vars vars1, vars1left, _)) :: rest671)) => let val  result = 
MlyValue.vars (fn _ => let val  (vars as vars1) = vars1 ()
 val  (var as var1) = var1 ()
 in (vars @ [var])
end)
 in ( LrTable.NT 5, ( result, vars1left, var1right), rest671)
end
|  ( 10, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ID ID1, _
, _)) :: ( _, ( MlyValue.tipe tipe1, tipe1left, _)) :: rest671)) =>
 let val  result = MlyValue.var (fn _ => let val  (tipe as tipe1) = 
tipe1 ()
 val  (ID as ID1) = ID1 ()
 in ({	ty = tipe, 
							id = ID })
end)
 in ( LrTable.NT 6, ( result, tipe1left, SEMICOLON1right), rest671)

end
|  ( 11, ( rest671)) => let val  result = MlyValue.methods (fn _ => (
[]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.method method1, _, method1right)) :: ( _, (
 MlyValue.methods methods1, methods1left, _)) :: rest671)) => let val 
 result = MlyValue.methods (fn _ => let val  (methods as methods1) = 
methods1 ()
 val  (method as method1) = method1 ()
 in (methods @ [method])
end)
 in ( LrTable.NT 7, ( result, methods1left, method1right), rest671)

end
|  ( 13, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements1, _, _)) :: ( _, ( MlyValue.vars vars1, _, _)) :: _ :: _ ::
 ( _, ( MlyValue.formalList formalList1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.tipe tipe1, _, _)) :: ( _, 
( _, (PUBLICleft as PUBLIC1left), _)) :: rest671)) => let val  result
 = MlyValue.method (fn _ => let val  (tipe as tipe1) = tipe1 ()
 val  (ID as ID1) = ID1 ()
 val  (formalList as formalList1) = formalList1 ()
 val  (vars as vars1) = vars1 ()
 val  (statements as statements1) = statements1 ()
 in (
	Method.T{
						name = ID,
						prefix = "",
						rettype = tipe,
						args = formalList,
						locals = vars,
						stms = statements,
						pos = PUBLICleft
						}
					      
)
end)
 in ( LrTable.NT 8, ( result, PUBLIC1left, RBRACE1right), rest671)
end
|  ( 14, ( rest671)) => let val  result = MlyValue.formalList (fn _ =>
 ([]))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( MlyValue.formalRests formalRests1, _, 
formalRests1right)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( 
MlyValue.tipe tipe1, tipe1left, _)) :: rest671)) => let val  result = 
MlyValue.formalList (fn _ => let val  (tipe as tipe1) = tipe1 ()
 val  ID1 = ID1 ()
 val  (formalRests as formalRests1) = formalRests1 ()
 in ({ty = tipe, 
				                   id = ID1} :: formalRests)
end
)
 in ( LrTable.NT 15, ( result, tipe1left, formalRests1right), rest671)

end
|  ( 16, ( rest671)) => let val  result = MlyValue.formalRests (fn _
 => ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.formalRest formalRest1, _, formalRest1right
)) :: ( _, ( MlyValue.formalRests formalRests1, formalRests1left, _))
 :: rest671)) => let val  result = MlyValue.formalRests (fn _ => let
 val  (formalRests as formalRests1) = formalRests1 ()
 val  (formalRest as formalRest1) = formalRest1 ()
 in (formalRests @ [formalRest])
end)
 in ( LrTable.NT 14, ( result, formalRests1left, formalRest1right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.tipe tipe1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671))
 => let val  result = MlyValue.formalRest (fn _ => let val  (tipe as 
tipe1) = tipe1 ()
 val  ID1 = ID1 ()
 in ({ty = tipe,
						     id = ID1})
end)
 in ( LrTable.NT 13, ( result, COMMA1left, ID1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RBRACK1right)) :: _ :: ( _, ( _, INT1left, _))
 :: rest671)) => let val  result = MlyValue.tipe (fn _ => (
 Tipe.ArrayInt))
 in ( LrTable.NT 11, ( result, INT1left, RBRACK1right), rest671)
end
|  ( 20, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.tipe (fn _ => ( Tipe.Int))
 in ( LrTable.NT 11, ( result, INT1left, INT1right), rest671)
end
|  ( 21, ( ( _, ( _, BOOLEAN1left, BOOLEAN1right)) :: rest671)) => let
 val  result = MlyValue.tipe (fn _ => ( Tipe.Bool))
 in ( LrTable.NT 11, ( result, BOOLEAN1left, BOOLEAN1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.tipe (fn _ => let val  ID1 = ID1 ()
 in ( Tipe.Name ID1)
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 23, ( rest671)) => let val  result = MlyValue.statements (fn _ =>
 ([]))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.statements statements1, _, statements1right
)) :: ( _, ( MlyValue.statement statement1, statement1left, _)) :: 
rest671)) => let val  result = MlyValue.statements (fn _ => let val  (
statement as statement1) = statement1 ()
 val  (statements as statements1) = statements1 ()
 in (statement @ statements)
end)
 in ( LrTable.NT 9, ( result, statement1left, statements1right), 
rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.statement (fn _ => let val  (statements as 
statements1) = statements1 ()
 in (statements)
end)
 in ( LrTable.NT 10, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.statement statement2, _, statement2right))
 :: _ :: ( _, ( MlyValue.statement statement1, _, _)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, (IFleft as IF1left), _))
 :: rest671)) => let val  result = MlyValue.statement (fn _ => let
 val  (exp as exp1) = exp1 ()
 val  statement1 = statement1 ()
 val  statement2 = statement2 ()
 in ([ Stm.If(exp, statement1, statement2, IFleft)])
end)
 in ( LrTable.NT 10, ( result, IF1left, statement2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, (WHILEleft
 as WHILE1left), _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (exp as exp1) = exp1 ()
 val  (statement as statement1) = statement1 ()
 in ([ Stm.While(exp, statement, WHILEleft)])
end)
 in ( LrTable.NT 10, ( result, WHILE1left, statement1right), rest671)

end
|  ( 28, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: ( _, ( MlyValue.exp 
exp1, _, _)) :: _ :: ( _, ( _, (SYSTEM_OUT_PRINTLNleft as 
SYSTEM_OUT_PRINTLN1left), _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (exp as exp1) = exp1 ()
 in ([ Stm.Print(exp, SYSTEM_OUT_PRINTLNleft)])
end)
 in ( LrTable.NT 10, ( result, SYSTEM_OUT_PRINTLN1left, 
SEMICOLON1right), rest671)
end
|  ( 29, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.exp exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, IDleft, _)) :: _ :: ( _, ( _, 
THIS1left, _)) :: rest671)) => let val  result = MlyValue.statement
 (fn _ => let val  ID1 = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
[ Stm.AssignId {id = ID1,
						       e = exp,
						       pointer = "this",
						       pos = IDleft}]
)
end)
 in ( LrTable.NT 10, ( result, THIS1left, SEMICOLON1right), rest671)

end
|  ( 30, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.exp exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.statement (fn _ => let val  
ID1 = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
[ Stm.AssignId {id = ID1,
					                                 e = exp,
					                                 pointer = "",
					                                 pos = IDleft}]
)
end)
 in ( LrTable.NT 10, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 31, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.exp exp2,
 _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, IDleft, _)) :: _ :: ( _, ( _, THIS1left, _)) :: 
rest671)) => let val  result = MlyValue.statement (fn _ => let val  
ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
[ Stm.AssignArray {id = ID1,
								  left = exp1,
								  right = exp2,
								  pointer = "this",
								  pos = IDleft}]
)
end)
 in ( LrTable.NT 10, ( result, THIS1left, SEMICOLON1right), rest671)

end
|  ( 32, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.exp exp2,
 _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.statement (fn _ => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
[ Stm.AssignArray {id = ID1,
							      left = exp1,
							      right = exp2,
							      pointer = "",
							      pos = IDleft}]
)
end)
 in ( LrTable.NT 10, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 33, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.exp exp1,
 _, _)) :: ( _, ( _, (RETURNleft as RETURN1left), _)) :: rest671)) =>
 let val  result = MlyValue.statement (fn _ => let val  (exp as exp1)
 = exp1 ()
 in ( [Stm.Return(exp, RETURNleft)])
end)
 in ( LrTable.NT 10, ( result, RETURN1left, SEMICOLON1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.And, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Or, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Plus, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Minus, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Times, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Mod, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Div, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Lt, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Gt, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Le, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Ge, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Neq, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Binop(exp1, Binop.Eq, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 47, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in ( Exp.Array(exp1, exp2, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, RBRACK1right), rest671)
end
|  ( 48, ( ( _, ( _, _, LENGTH1right)) :: _ :: ( _, ( MlyValue.exp 
exp1, (expleft as exp1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ( Exp.Length(exp, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, LENGTH1right), rest671)
end
|  ( 49, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expList 
expList1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, (
 MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 val  ID1 = ID1 ()
 val  (expList as expList1) = expList1 ()
 in ( Exp.Call(exp,  Tipe.Int, ID1,  Tipe.Int, expList, expleft))
end)
 in ( LrTable.NT 12, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 50, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expList 
expList1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (expList as expList1) = expList1 ()
 in ( Exp.SimpleCall(ID, Tipe.Int, expList, IDleft))
end)
 in ( LrTable.NT 12, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.INTEGER_LITERAL INTEGER_LITERAL1, 
INTEGER_LITERAL1left, INTEGER_LITERAL1right)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  (INTEGER_LITERAL as 
INTEGER_LITERAL1) = INTEGER_LITERAL1 ()
 in ( Exp.Inti(INTEGER_LITERAL))
end)
 in ( LrTable.NT 12, ( result, INTEGER_LITERAL1left, 
INTEGER_LITERAL1right), rest671)
end
|  ( 52, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => ( Exp.True))
 in ( LrTable.NT 12, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 53, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => ( Exp.False))
 in ( LrTable.NT 12, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (
 Exp.Var{id = ID,
				              pointer = "",
				              pos = IDleft}
)
end)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 55, ( ( _, ( _, (THISleft as THIS1left), THIS1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => ( Exp.This THISleft))
 in ( LrTable.NT 12, ( result, THIS1left, THIS1right), rest671)
end
|  ( 56, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: _ :: ( _, ( _, (NEWleft as NEW1left), _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1
 ()
 in ( Exp.NewArray(exp, NEWleft))
end)
 in ( LrTable.NT 12, ( result, NEW1left, RBRACK1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (NEWleft as NEW1left), _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  ID1 = ID1 ()
 in ( Exp.NewId(ID1, NEWleft))
end)
 in ( LrTable.NT 12, ( result, NEW1left, RPAREN1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
NOTleft as NOT1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ( Exp.Not(exp, NOTleft))
end)
 in ( LrTable.NT 12, ( result, NOT1left, exp1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, (LPARENleft as LPAREN1left), _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ( Exp.ParenExp(exp, LPARENleft) )
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 60, ( rest671)) => let val  result = MlyValue.expList (fn _ => (
[]))
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 61, ( ( _, ( MlyValue.expRests expRests1, _, expRests1right)) :: 
( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.expList (fn _ => let val  (exp as exp1) = exp1 ()
 val  (expRests as expRests1) = expRests1 ()
 in (exp::expRests)
end)
 in ( LrTable.NT 18, ( result, exp1left, expRests1right), rest671)
end
|  ( 62, ( rest671)) => let val  result = MlyValue.expRests (fn _ => (
[]))
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 63, ( ( _, ( MlyValue.expRest expRest1, _, expRest1right)) :: ( _
, ( MlyValue.expRests expRests1, expRests1left, _)) :: rest671)) =>
 let val  result = MlyValue.expRests (fn _ => let val  (expRests as 
expRests1) = expRests1 ()
 val  (expRest as expRest1) = expRest1 ()
 in (expRests@[expRest])
end)
 in ( LrTable.NT 16, ( result, expRests1left, expRest1right), rest671)

end
|  ( 64, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
COMMA1left, _)) :: rest671)) => let val  result = MlyValue.expRest (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 17, ( result, COMMA1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID'
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Java_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID',p1,p2))
fun CLASS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID',p1,p2))
fun MAIN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID',p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID',p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID',p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID',p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID',p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID',p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID',p1,p2))
fun STATIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID',p1,p2))
fun PUBLIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID',p1,p2))
fun VOID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID',p1,p2))
fun STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID',p1,p2))
fun EXTENDS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID',p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID',p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID',p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID',p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID',p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID',p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID',p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID',p1,p2))
fun BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID',p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID',p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID',p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID',p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID',p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID',p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID',p1,p2))
fun THIS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID',p1,p2))
fun INTEGER_LITERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.INTEGER_LITERAL (fn () => i),p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID',p1,p2))
fun LENGTH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID',p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID',p1,p2))
fun SYSTEM_OUT_PRINTLN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33
,(ParserData.MlyValue.VOID',p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID',p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID',p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID',p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID',p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID',p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID',p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID',p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID',p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID',p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID',p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID',p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
