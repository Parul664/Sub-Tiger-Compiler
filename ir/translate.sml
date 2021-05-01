
signature ENV =
sig
  type mp
  val lookup : string * mp -> Temp.temp option
  val update : string * Temp.temp * mp -> mp
  val empty : unit -> mp
end


structure env :ENV = 
struct
    structure CUS_MAP = RedBlackMapFn (struct
                            type ord_key = string
                            val	compare = String.compare
                        end)    
    type ret = Temp.temp
    type mp = ret CUS_MAP.map
    val MP : mp = CUS_MAP.empty

    fun empty () = CUS_MAP.empty 

    fun lookup (s, m) = (CUS_MAP.find (m, s))                              
    
    fun update (s, i, m) = CUS_MAP.insert(m, s, i )
end


structure Translate :sig 
    val translist : Tiger.exp list -> Tree.exp list
end = 
struct


    exception Unsupported of string

    structure Tig = Tiger
    structure Temp = Temp
    structure T = Tree
    
    datatype exp = Ex of T.exp
                 | Nx of T.stm 
                 | Cx of Temp.label * Temp.label -> T.stm

    (* This functions strips of whatever constructor 
    is there to obtain a expression 
    Type : exp -> Tree.exp
    *)
    fun unEx (Ex e)      = e 
      | unEx (Nx s)      = T.ESEQ(s,T.CONST 0)
      | unEx (Cx genstm) = let val r = Temp.newtemp()
                               val t = Temp.newlabel()
                               val f = Temp.newlabel()
                           in
                                T.ESEQ(
                                    T.SEQ (T.MOVE(T.TEMP r, T.CONST 1),
                                    T.SEQ (genstm(t,f),
                                    T.SEQ (T.LABEL f,
                                    T.SEQ (T.MOVE(T.TEMP r, T.CONST 0),
                                    T.LABEL t))))
                                , T.TEMP r)
                           end

    (* This function will convert whatever comes its way into and expression *)
    fun unNx (Ex e)      = T.EXP e
      | unNx (Nx s)      = s
      | unNx (Cx genstm) = T.EXP (unEx (Cx genstm))

    (* Remove the statement that look like conditional *)
    fun unCx (Ex e)      = (fn ((t:Temp.label),(f:Temp.label)) => T.CJUMP(T.EQ, T.CONST 0, e ,t,f))
      | unCx (Nx s)      = (TextIO.output(TextIO.stdErr, "Not a requred conversion\n"); OS.Process.exit OS.Process.failure)
      | unCx (Cx genstm) = genstm



    fun TranslateExp e = ( case e of
      
        Tig.NilExpr => raise Unsupported "Nil Expression" 
      | Tig.SExpr s => raise Unsupported "String Expression"
      | Tig.IExpr i => T.CONST i
      | Tig.ArrayExpr {Type, Sizee, Initial} => raise Unsupported "Array Expression"
      | Tig.RecordExpr {Type, Values}        => raise Unsupported "Array Expression"
      | Tig.LValueExpr (lval)    => raise Unsupported "Array Expression"
      | Tig.FuncCallExpr {Id, Param} => raise Unsupported "Array Expression"
      | Tig.NegationExpr e => raise Unsupported "Array Expression"
      | Tig.BinOpExpr (e1,opp, e2) => (binOpManager e1 e2 opp)
      | Tig.SeqExpr el => raise Unsupported "Array Expression"
      | Tig.AssignExpr (lval, e) => raise Unsupported "Array Expression"
      | Tig.IfExpr {Cond, Do,Else } => raise Unsupported "Array Expression"
      | Tig.WhileExpr {Cond, Do}  => raise Unsupported "Array Expression"
      | Tig.ForExpr {Id, StartPos, EndPos,Do} => raise Unsupported "Array Expression"
      | Tig.BreakExpr => raise Unsupported "Array Expression"
      | Tig.LetExpr (dl, el) => raise Unsupported "Array Expression"
      ) 

    and binOpManager e1 e2 opp = let val te1 = TranslateExp e1
                                     val te2 = TranslateExp e2 in 
            (case opp of 
              Tig.Plus           => T.BINOP (T.PLUS , te1, te2 )
            | Tig.Minus          => T.BINOP( T.MINUS, te1, te2 ) 
            | Tig.Mul            => T.BINOP( T.MUL  , te1, te2 )
            | Tig.Div            => T.BINOP( T.DIV  , te1, te2 )
            | Tig.And            => T.BINOP( T.AND  , te1, te2 )
            | Tig.Or             => T.BINOP( T.AND  , te1, te2 )
            | Tig.LessEqualThan  => unEx(Cx(fn(t,f) => T.CJUMP(T.LE,te1, te2,t,f)))
            | Tig.GreatEqualThan => unEx(Cx(fn(t,f) => T.CJUMP(T.GE,te1, te2,t,f)))
            | Tig.LessThan       => unEx(Cx(fn(t,f) => T.CJUMP(T.LT,te1, te2,t,f)))
            | Tig.GreatThan      => unEx(Cx(fn(t,f) => T.CJUMP(T.GT,te1, te2,t,f)))
            | Tig.AngBrac        => unEx(Cx(fn(t,f) => T.CJUMP(T.NE,te1, te2,t,f)))
            | Tig.Equals         => unEx(Cx(fn(t,f) => T.CJUMP(T.EQ,te1, te2,t,f)))
            )end


    fun translist []        = [] 
      | translist (e :: el) = ((TranslateExp e) :: (translist el))


end
