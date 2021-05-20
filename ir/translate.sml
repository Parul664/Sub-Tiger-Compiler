
signature ENV =
sig
  type mp
  type ret
  val lookup : string * mp -> ret option
  val update : string * ret * mp -> mp
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


structure envFn :ENV = 
struct

    structure CUS_MAP = RedBlackMapFn (struct 
		      								  type ord_key = string 
											      val	compare = String.compare 
										      end)    
    type ret = (Temp.label*int)
    type mp = ret CUS_MAP.map
    val MP : mp = CUS_MAP.empty

    fun empty () = CUS_MAP.empty 

    fun lookup (s, m) = (CUS_MAP.find (m, s))                              
    
    fun update (s, i, m) = CUS_MAP.insert(m, s, i )
end

signature FRAME = sig
    type pts
    val sp : Temp.temp
    val fp : Temp.temp
    val ra : Temp.temp
    val wordsize : int
end

structure frame : FRAME = struct
    type pts = Temp.temp
    val sp: pts = 0
    val fp: pts = 1
    val ra: pts = 2 
    val wordsize = 8
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
    structure EVF = envFn
    structure F = frame

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



    fun TranslateExp e envv envf = ( case e of
      
        (Tig.NilExpr)                           => raise Unsupported "Nil Expression" 
      | (Tig.SExpr s)                           => raise Unsupported "String Expression"
      | (Tig.IExpr i)                           => Ex(T.CONST i)
      | (Tig.ArrayExpr {Type, Sizee, Initial})  => raise Unsupported "Array Expression"
      | (Tig.RecordExpr {Type, Values})         => raise Unsupported "Array Expression"
      | (Tig.LValueExpr (lval))                 => lValueExprManager envv lval envf
      | (Tig.FuncCallExpr {Id, Param})          => FunCallManager Id Param envv envf
      | (Tig.NegationExpr e)                    => Ex (T.BINOP (T.MINUS, T.CONST 0, unEx (TranslateExp e envv envf) ))
      | (Tig.BinOpExpr (e1,opp, e2))            => binOpManager e1 e2 opp envv envf
      | (Tig.SeqExpr el)                        => ExpListManager envv el envf
      | (Tig.AssignExpr (lval, e))              => Nx (T.MOVE(unEx(lValueExprManager envv lval envf), unEx (TranslateExp e envv envf) ))
      | (Tig.IfExpr {Cond, Do,Else })           => ifManager Cond Do Else envv envf
      | (Tig.WhileExpr {Cond, Do})              => whileManager Cond Do envv envf
      | (Tig.ForExpr {Id, StartPos, EndPos,Do}) => forManager Id StartPos EndPos Do envv envf
      | (Tig.BreakExpr)                         => breakManager ()
      | (Tig.LetExpr (dl, el))                  => letExprManager envv dl el envf
      ) 


    
    and forManager Id StartPos EndPos Do env envf= let val a = Tig.LetExpr([Tig.VarDec(Id,NONE,StartPos)],
                                                            [Tig.WhileExpr{Cond=  Tig.BinOpExpr(Tig.LValueExpr(Tig.SimpleVar(Id)),Tig.LessEqualThan,EndPos),
                                                            Do = Tig.SeqExpr([Do,Tig.AssignExpr(Tig.SimpleVar Id,Tig.BinOpExpr(Tig.LValueExpr(Tig.SimpleVar Id),Tig.Plus,Tig.IExpr(1)))])
                                                            }]) in
                                                (TranslateExp a env envf)
                                                end


    and whileManager Cond Do env envf= let fun rm [] = []
                                         | rm (l::ls) = ls                                       
                                       val test  = Temp.newlabel()
                                       val t =  Temp.newlabel()
                                       val done  = Temp.newlabel()
                                       val Condd = unCx(TranslateExp Cond env envf)
                                       val Doo   = (LoopList := (done::(!LoopList));unNx(TranslateExp Do env envf))
                                       in (LoopList := (rm (!LoopList)) ;
                                       Nx(T.SEQ(T.LABEL test, T.SEQ(Condd(t,done), T.SEQ(T.LABEL t,
                                       T.SEQ(Doo,T.SEQ(T.JUMP(T.NAME test,[test]), T.LABEL done))))))
                                       )
                                      end
    
    and breakManager () = (case !LoopList of 
                          []      => raise InvalidBreak "Invalid Break"  
                        | (l::ls) => Nx(T.JUMP(T.NAME l,[l])) 
                        )

    and ifManager Cond Do Else convv envf= let val Condd = unCx (TranslateExp Cond convv envf)
                                           val Doo = unEx (TranslateExp Do convv envf)                                            
                                           val t = Temp.newlabel()
                                           val f = Temp.newlabel()
                                           val r = Temp.newtemp() 
                                           val join = Temp.newlabel()
                                       in (case Else of
                                       NONE => Ex(T.ESEQ(T.SEQ(Condd (t,f),(T.SEQ(T.LABEL t, T.SEQ(T.MOVE (T.TEMP r,Doo), T.SEQ(T.JUMP(T.NAME join,[join]), 
                                                          T.SEQ( T.LABEL f, T.SEQ(T.MOVE (T.TEMP r,T.CONST 0), T.LABEL join))))))),T.TEMP r))
                                      | SOME(elsee) => let val Elsee = unEx (TranslateExp elsee convv envf) in 
                                                          Ex(T.ESEQ(T.SEQ(Condd (t,f),(T.SEQ(T.LABEL t, T.SEQ(T.MOVE (T.TEMP r,Doo), T.SEQ(T.JUMP(T.NAME join,[join]), 
                                                          T.SEQ( T.LABEL f, T.SEQ(T.MOVE (T.TEMP r,Elsee), T.LABEL join))))))),T.TEMP r))
                                                      end
                                       )end


    and letExprManager envv dl el envf= let val newEnvv = updateEnvList envv dl envf
                                    val left = ExpListManager (#2(newEnvv)) el (#3(newEnvv))
                                    in 
                                      Ex(T.ESEQ((#1(newEnvv)), unEx(left)))
                                    end

    and updateEnvList envv l envf = let fun checkAndUpdateEnv env id e envf= let val nw = Temp.newtemp() 
                                                                      val newEnv = EV.update (id, nw, env)    
                                                                      in (T.MOVE (T.TEMP nw, unEx(TranslateExp e env envf )), newEnv ,envf) 
                                                                      end
                                    fun updateEnv d env envf=  (case d of 
                                        Tig.TypeDec (id, ty) => raise Unsupported "TypeDec"
                                      | Tig.VarDec (id, NONE, exp) => checkAndUpdateEnv env id exp envf
                                      | Tig.VarDec (id, (SOME(Tig.Sub("int"))), exp ) => checkAndUpdateEnv env id exp envf
                                      | Tig.VarDec (id, SOME(_), exp)    => raise Unsupported "VarDecNotInt"
                                      | Tig.FuncDecs (a) => FuncManager env envf a 
                                    )
                                in (case l of 
                                  []  =>  (T.EXP(T.CONST 0), envv, envf)
                                | (l1::ls) => let val a = (updateEnv l1 envv envf)  
                                                  val n = (updateEnvList (#2(a)) ls (#3(a))) in 
                                                  (T.SEQ(#1(a), #1(n)), #2(n), #3(n))
                                              end
                                  )end

    and FuncManager env envf (Tig.fundec{Id, Param, RetType, Body}) = 
                                    let 
                                    val nl = Temp.newlabel() 
                                    fun rmside (Tig.TF(l)) = l
                                    fun count [] = 0
                                      | count (a::ab) = 1 + (count ab)
                                    val nenvf = EVF.update (Id, (nl,(count (rmside(Param)))),envf)
                                    fun checkAndUpdateEnv env id pos = let val nw = Temp.newtemp() 
                                                                      val newEnv = EV.update (id, nw, env)    
                                                                      in (T.MOVE (T.TEMP nw, T.MEM(T.BINOP(T.PLUS, T.TEMP F.fp,T.CONST pos))), newEnv ) 
                                                                      end
                                    fun cl env [] pos              = (T.EXP(T.CONST 0), env)
                                      | cl env ( (id, tyid)::lst) pos = ( case tyid of Tig.Sub "int" =>
                                                                        let val (st,nenv) = (checkAndUpdateEnv env id pos)
                                                                            val (et,nenvv) = cl nenv lst (pos+F.wordsize) 
                                                                        in (T.SEQ(st,et),nenvv)
                                                                        end
                                                                        | _ => (TextIO.output(TextIO.stdErr,"Parameter "^id^" passed to function "^Id^"() must be int\n");
                                                                              raise Unsupported "Params should be int")
                                                                        )
                                    
                                    fun Updat (Tig.TF(l)) envv = cl envv l 0
                                    val (stps, newEnv) = Updat Param env
                                    val ComBody = (TranslateExp Body newEnv nenvf)
                                    val raa = T.TEMP F.ra
                                    val ffp = T.TEMP F.fp
                                    val ssp = T.TEMP F.sp
                                    val te  = T.TEMP (Temp.newtemp())
                                    val rt = T.TEMP (Temp.newtemp())
                                    val w = F.wordsize
                                    val irgen = (T.SEQ(T.LABEL nl,  T.SEQ(T.MOVE(T.MEM(T.BINOP(T.MINUS, ssp, T.CONST w)),raa), 
                                                T.SEQ(T.MOVE(T.MEM(T.BINOP(T.MINUS, ssp, T.CONST (2*w))),ffp),
                                                T.SEQ(T.MOVE(ffp,ssp), T.SEQ(T.MOVE(ssp,T.BINOP(T.MINUS,ssp,T.CONST (2*w))),
                                                T.SEQ(stps,T.SEQ(T.MOVE(te,unEx(ComBody)), T.SEQ(T.MOVE(ssp,ffp), 
                                                T.SEQ(T.MOVE(ffp,T.MEM(T.BINOP(T.MINUS,ssp,T.CONST (2*w)))),
                                                T.SEQ(T.MOVE(rt, T.MEM(T.BINOP(T.MINUS,ssp,T.CONST w))),
                                                T.SEQ(T.MOVE(T.MEM(T.BINOP(T.MINUS,ssp,T.CONST w)),te) ,T.JUMP(rt,[])))))))))))))
                                    in 
                                    (case RetType of 
                                        NONE => (irgen, env, nenvf)
                                      | SOME x => (case x of 
                                            (Tig.Sub("int")) => (irgen, env, nenvf)
                                            | _   => ((TextIO.output(TextIO.stdErr,"Return Type of "^Id^"() must be an int\n"));
                                                      (raise Unsupported "ReturnTypeInt") )
                                          )
                                    )
                                    end 
    (* Calling of function *)
    and FunCallManager id param env envf = let
                                          fun count [] = 0
                                          | count (a::ab) = 1 + (count ab)
                                          fun ManageExp a = TranslateExp a env envf
                                          fun ManageList [] = []
                                            | ManageList (a::ass) = (unEx(TranslateExp a env envf)::(ManageList ass))
                                          in 
                                          (case EVF.lookup(id, envf) of 
                                            NONE   => (TextIO.output(TextIO.stdErr,"Function "^id^"() not defined\n");
                                                      raise NotDefined "Function Not Defined")
                                          | SOME x => ( if (#2(x) <> (count(param))) then 
                                                        (TextIO.output(TextIO.stdErr,"Wrong Number of Parameters passed to "^id^"()\n");
                                                        raise Unsupported "WrongParam")
                                                        else Ex(T.CALL(T.NAME (#1(x)), ManageList param))
                                            )
                                          )
                                          
                                          end
  

    and ExpListManager envv [] envf  = Ex(T.CONST 0)
      | ExpListManager envv [l] envf = (TranslateExp l envv envf)
      | ExpListManager envv (l::ls) envf = Ex(T.ESEQ(unNx (TranslateExp l envv envf), unEx(ExpListManager envv ls envf)))  
                                

    (* LValue means it is taking the value of something, that already exists and 
    so we'll have to check for its existance in map *)
    and lValueExprManager envv lval envf = 
    (case lval of 
        Tig.SimpleVar id => 
                        (case EV.lookup(id,envv) of 
                          NONE => (TextIO.output(TextIO.stdErr,("Undefined variable " ^ id^ "\n"));
                                raise NotDefined ("Undefined variable"))
                        | SOME (e) => Ex (T.TEMP e)
                         )
      | Tig.FeildVar (lval, id) => raise Unsupported "FeildVar"
      | Tig.ArrEl (lval, e)  => raise Unsupported "ArrEl"
    )

    
    and binOpManager e1 e2 opp envv envf = let val te1 = unEx(TranslateExp e1 envv envf)
                                               val te2 = unEx(TranslateExp e2 envv envf) in 
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

        fun preDefinedFxns m = let val a = EVF.update ("print", (Temp.newlabel(),1),m)
                               val b = EVF.update ("flush", (Temp.newlabel(),0),a)
                               val c = EVF.update ("getchar", (Temp.newlabel(),0),b)
                               val d = EVF.update ("ord", (Temp.newlabel(),1),c)
                               val e = EVF.update ("chr", (Temp.newlabel(),1),d)
                               val f = EVF.update ("size", (Temp.newlabel(),1),e)
                               val g = EVF.update ("substring", (Temp.newlabel(),3),f)
                               val h = EVF.update ("concat", (Temp.newlabel(),2),g)
                               val i = EVF.update ("not", (Temp.newlabel(),1),h)
                               val j = EVF.update ("exit", (Temp.newlabel(),1),i)
                            in j
                            end


    fun transList []        = T.EXP(T.CONST 0) 
      | transList (l :: ls) = T.SEQ(unNx(TranslateExp l (EV.empty()) (preDefinedFxns(EVF.empty()))), transList ls)


end
