
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
    val transList : Tiger.exp list -> Tree.stm
end = 
struct


    exception Unsupported of string
    exception InvalidBreak of string
    exception NotDefined of string

    structure Tig = Tiger
    structure Temp = Temp
    structure T = Tree
    structure EV = env
    val LoopList : Temp.label list ref = ref []
    
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



    fun TranslateExp e envv = ( case e of
      
        (Tig.NilExpr)                           => raise Unsupported "Nil Expression" 
      | (Tig.SExpr s)                           => raise Unsupported "String Expression"
      | (Tig.IExpr i)                           => Ex(T.CONST i)
      | (Tig.ArrayExpr {Type, Sizee, Initial})  => raise Unsupported "Array Expression"
      | (Tig.RecordExpr {Type, Values})         => raise Unsupported "Array Expression"
      | (Tig.LValueExpr (lval))                 => lValueExprManager envv lval
      | (Tig.FuncCallExpr {Id, Param})          => raise Unsupported "Array Expression"
      | (Tig.NegationExpr e)                    => Ex (T.BINOP (T.MINUS, T.CONST 0, unEx (TranslateExp e envv) ))
      | (Tig.BinOpExpr (e1,opp, e2))            => binOpManager e1 e2 opp envv
      | (Tig.SeqExpr el)                        => ExpListManager envv el
      | (Tig.AssignExpr (lval, e))              => Nx (T.MOVE(unEx(lValueExprManager envv lval), unEx (TranslateExp e envv) ))
      | (Tig.IfExpr {Cond, Do,Else })           => ifManager Cond Do Else envv
      | (Tig.WhileExpr {Cond, Do})              => whileManager Cond Do envv
      | (Tig.ForExpr {Id, StartPos, EndPos,Do}) => forManager Id StartPos EndPos Do envv
      | (Tig.BreakExpr)                         => breakManager ()
      | (Tig.LetExpr (dl, el))                  => letExprManager envv dl el
      ) 


    
    and forManager Id StartPos EndPos Do env = let val a = Tig.LetExpr([Tig.VarDec(Id,NONE,StartPos)],
                                                            [Tig.WhileExpr{Cond=  Tig.BinOpExpr(Tig.LValueExpr(Tig.SimpleVar(Id)),Tig.LessEqualThan,EndPos),
                                                            Do = Tig.SeqExpr([Do,Tig.AssignExpr(Tig.SimpleVar Id,Tig.BinOpExpr(Tig.LValueExpr(Tig.SimpleVar Id),Tig.Plus,Tig.IExpr(1)))])
                                                            }]) in
                                                (TranslateExp a env)
                                                end


    and whileManager Cond Do env = let fun rm [] = []
                                         | rm (l::ls) = ls                                       
                                       val test  = Temp.newlabel()
                                       val t =  Temp.newlabel()
                                       val done  = Temp.newlabel()
                                       val Condd = unCx(TranslateExp Cond env)
                                       val Doo   = (LoopList := (done::(!LoopList));unNx(TranslateExp Do env))
                                       in (LoopList := (rm (!LoopList)) ;
                                       Nx(T.SEQ(T.LABEL test, T.SEQ(Condd(t,done), T.SEQ(T.LABEL t,
                                       T.SEQ(Doo,T.SEQ(T.JUMP(T.NAME test,[test]), T.LABEL done))))))
                                       )
                                      end
    
    and breakManager () = (case !LoopList of 
                          []      => raise InvalidBreak "Invalid Break"  
                        | (l::ls) => Nx(T.JUMP(T.NAME l,[l])) 
                        )

    and ifManager Cond Do Else convv = let val Condd = unCx (TranslateExp Cond convv)
                                           val Doo = unEx (TranslateExp Do convv)                                            
                                           val t = Temp.newlabel()
                                           val f = Temp.newlabel()
                                           val r = Temp.newtemp() 
                                           val join = Temp.newlabel()
                                       in (case Else of
                                       NONE => Ex(T.ESEQ(T.SEQ(Condd (t,f),(T.SEQ(T.LABEL t, T.SEQ(T.MOVE (T.TEMP r,Doo), T.SEQ(T.JUMP(T.NAME join,[join]), 
                                                          T.SEQ( T.LABEL f, T.SEQ(T.MOVE (T.TEMP r,T.CONST 0), T.LABEL join))))))),T.TEMP r))
                                      | SOME(elsee) => let val Elsee = unEx (TranslateExp elsee convv) in 
                                                          Ex(T.ESEQ(T.SEQ(Condd (t,f),(T.SEQ(T.LABEL t, T.SEQ(T.MOVE (T.TEMP r,Doo), T.SEQ(T.JUMP(T.NAME join,[join]), 
                                                          T.SEQ( T.LABEL f, T.SEQ(T.MOVE (T.TEMP r,Elsee), T.LABEL join))))))),T.TEMP r))
                                                      end
                                       )end


    and letExprManager envv dl el = let val newEnvv = updateEnvList envv dl
                                    val left = ExpListManager (#2(newEnvv)) el
                                    in 
                                      Ex(T.ESEQ((#1(newEnvv)), unEx(left)))
                                    end

    and updateEnvList envv l  = let fun checkAndUpdateEnv env id e = let val nw = Temp.newtemp() 
                                                                      val newEnv = EV.update (id, nw, env)    
                                                                      in (T.MOVE (T.TEMP nw, unEx(TranslateExp e env )), newEnv ) 
                                                                      end
                                    fun updateEnv d env =  (case d of 
                                        Tig.TypeDec (id, ty) => raise Unsupported "TypeDec"
                                      | Tig.VarDec (id, NONE, exp) => checkAndUpdateEnv envv id exp
                                      | Tig.VarDec (id, (SOME(Tig.Sub("int"))), exp ) => checkAndUpdateEnv envv id exp
                                      | Tig.VarDec (id, SOME(_), exp)    => raise Unsupported "VarDecNotInt"
                                      | Tig.FuncDecs _ => raise Unsupported "FunDecs"
                                    )
                                in (case l of 
                                  []  =>  (T.EXP(T.CONST 0), envv)
                                | (l1::ls) => let val a = (updateEnv l1 envv)  
                                                  val n = (updateEnvList (#2(a)) ls) in 
                                                  (T.SEQ(#1(a), #1(n)), #2(n))
                                              end
                                  )end
    and ExpListManager envv [] = Ex(T.CONST 0)
      | ExpListManager envv [l] = (TranslateExp l envv)
      | ExpListManager envv (l::ls) = Ex(T.ESEQ(unNx (TranslateExp l envv), unEx(ExpListManager envv ls)))  
                                

    (* LValue means it is taking the value of something, that already exists and 
    so we'll have to check for its existance in map *)
    and lValueExprManager envv lval = 
    (case lval of 
        Tig.SimpleVar id => 
                        (case EV.lookup(id,envv) of 
                          NONE => raise NotDefined ("Undefined variable" ^ id)
                        | SOME (e) => Ex (T.TEMP e)
                         )
      | Tig.FeildVar (lval, id) => raise Unsupported "FeildVar"
      | Tig.ArrEl (lval, e)  => raise Unsupported "ArrEl"
    )

    
    and binOpManager e1 e2 opp envv = let val te1 = unEx(TranslateExp e1 envv)
                                     val te2 = unEx(TranslateExp e2 envv) in 
            (case opp of 
              Tig.Plus           => Ex(T.BINOP (T.PLUS , te1, te2 ))
            | Tig.Minus          => Ex(T.BINOP( T.MINUS, te1, te2 )) 
            | Tig.Mul            => Ex(T.BINOP( T.MUL  , te1, te2 ))
            | Tig.Div            => Ex(T.BINOP( T.DIV  , te1, te2 ))
            | Tig.And            => Ex(T.BINOP( T.AND  , te1, te2 ))
            | Tig.Or             => Ex(T.BINOP( T.AND  , te1, te2 ))
            | Tig.LessEqualThan  => (Cx(fn(t,f) => T.CJUMP(T.LE,te1, te2,t,f)))
            | Tig.GreatEqualThan => (Cx(fn(t,f) => T.CJUMP(T.GE,te1, te2,t,f)))
            | Tig.LessThan       => (Cx(fn(t,f) => T.CJUMP(T.LT,te1, te2,t,f)))
            | Tig.GreatThan      => (Cx(fn(t,f) => T.CJUMP(T.GT,te1, te2,t,f)))
            | Tig.AngBrac        => (Cx(fn(t,f) => T.CJUMP(T.NE,te1, te2,t,f)))
            | Tig.Equals         => (Cx(fn(t,f) => T.CJUMP(T.EQ,te1, te2,t,f)))
            )end


    fun transList []        = T.EXP(T.CONST 0) 
      | transList (l :: ls) = T.SEQ(unNx(TranslateExp l (EV.empty())), transList ls)


end
