structure PrintAbsyn :
sig val print : TextIO.outstream * Tiger.exp list -> unit end
= struct
structure A = Tiger

fun print(outstream, e0) = 
  let 
  fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 


  fun indent 0 = ()
    | indent i = (say " "; indent(i-1))


  fun opname A.Plus = "Plus"
    | opname A.Minus = "Minus"
    | opname A.Mul = "Mul"
    | opname A.Div = "Div"
    | opname A.Equals = "Equals"
    | opname A.AngBrac = "AngBrac"
    | opname A.GreatThan = "GreatThan"
    | opname A.LessThan = "LessThan"
    | opname A.GreatEqualThan = "GreatEqualThan"
    | opname A.LessEqualThan = "LessEqualThan"
    | opname A.And = "And"
    | opname A.Or = "Or"



  fun dolist d f [a] = (sayln ""; f(a,d+1))
    | dolist d f (a::r) = (sayln ""; f(a,d+1); say ","; dolist d f r)
    | dolist d f nil = ()


  fun var(A.SimpleVar(s),d)  = (indent d; say "SimpleVar("; 
			                          say(s); say ")")
    | var(A.FeildVar(v,s),d) = (indent d; sayln "FieldVar(";
				                        var(v,d+1); sayln ",";
				                        indent(d+1); say(s); say ")")
    | var(A.ArrEl(v,e),d)    = (indent d; sayln "ArrEl(";
				                        var(v,d+1); sayln ",";
				                        exp(e,d+1); say ")")

  and  exp(A.NilExpr, d) = (indent d; say "NilExpr")
     | exp(A.IExpr i, d) = (indent d; say "IExpr("; say(Int.toString i);
			                      say ")")
     | exp(A.SExpr(s),d) = (indent d; say "SExpr(\"";
				                    say s; say "\")")
     | exp(A.FuncCallExpr{Id,Param},d) = (indent d; say "FuncCallExpr("; say(Id);
                                         say ",["; dolist d exp Param; say "])")

     | exp(A.BinOpExpr(left,oper,right),d) =(indent d; say "BinOpExpr("; say(opname oper); sayln ",";
                                             exp(left,d+1); sayln ","; exp(right,d+1); say ")")

     | exp(A.RecordExpr{Type,Values},d) = let fun f((name,e),d) = 
                                                 (indent d; say "("; say(name);
                                                  sayln ","; exp(e,d+1);
                                                  say ")")
                                          in indent d; say "RecordExpr("; type_id(Type); 
                                             say ",["; dolist d f Values; say "])" 
                                          end
     | exp(A.LValueExpr(lv),d)  = (indent d; sayln "LValueExpr(";var(lv,d);say")")
     | exp(A.NegationExpr e, d) = (indent d;say "NegationExpr("; exp(e,d+1); say")")
     | exp(A.SeqExpr l, d)      = (indent d; say "SeqExpr["; dolist d exp l; 
			                             say "]")
     | exp(A.AssignExpr(v,e),d) = (indent d; sayln "AssignExpr("; var(v,d+1); sayln ",";
		                               exp(e,d+1); say ")")
     | exp(A.IfExpr{Cond,Do,Else},d) = (indent d; sayln "IfExpr("; exp(Cond,d+1); sayln ",";
		                                   exp(Do,d+1);
                                       case Else of NONE => ()
                                         | SOME e => (sayln ","; exp(e,d+1));
                                       say ")")
     | exp(A.WhileExpr{Cond,Do},d) =
		                                  (indent d; sayln "WhileExpr("; exp(Cond,d+1); sayln ",";
		                                   exp(Do,d+1); say ")")
     | exp(A.ForExpr{Id,StartPos,EndPos,Do},d) = (indent d; sayln "ForExpr(";
                                                  say(Id); say ",";exp(StartPos,d+1);
                                                  sayln ","; exp(EndPos,d+1); 
                                                  sayln ","; exp(Do,d+1); say ")")
     | exp(A.BreakExpr, d) = (indent d; say "BreakExpr")
     | exp(A.LetExpr(decs,body),d) =(indent d; say "LetExpr([";
		                                 dolist d dec decs; say "],"; 
                                     dolist d exp body; say")")
     | exp(A.ArrayExpr{Type,Sizee,Initial},d) = (indent d; say "ArrayExpr("; 
                                                type_id(Type); sayln ",";
		                                            exp(Sizee,d+1); sayln ","; 
                                                exp(Initial,d+1); say ")")
 

  and dec(A.FuncDecs l, d) = let
                             fun temp (A.TF(t)) = t 
                             fun field((name,typ),d) = 
			                       (indent d;say(name);
			                        say ","; type_id(typ))
		                         fun f(A.fundec{Id,Param,RetType,Body},d) =
		                         (indent d; say "fundec("; say (Id); say ",TF[";
		                         (dolist d field (temp Param));indent d ;sayln "],";indent d;
		                         case RetType of NONE => sayln "NONE"
			                          | SOME(s) => (say "SOME("; type_id(s); sayln "),");
		                             exp(Body,d+1); say ")")
	                           in indent d; say "FuncDecs("; f (l,d); say ")"
	                           end

    | dec(A.VarDec(name,typ,init),d) = (indent d; say "VarDec("; say(name); say ",";
	                                     case typ of NONE => say "NONE" 
		                                   | SOME(s)=> (say "SOME("; type_id( s); say ")");
                                       sayln ","; exp(init,d+1); say ")")

    | dec(A.TypeDec (id,typ), d) = (indent d; say "TypeDec["; ty (typ,d); say "]")
         
   
  and ty(A.AlreadyTy(s), d) = (indent d; say "AlreadyTy("; type_id(s);
			                         say ")")
    | ty(A.SeqTy seq, d) = let 
                           fun temp (A.TF(t)) = t 
                           fun f((name,typ),d) =
			                     (indent d; say "("; say (name); say ",";
			                     type_id ( typ); say ")")
	                         in indent d; say "SeqTy["; 
                           (dolist d f (temp seq)); say "]"
		                       end
    | ty(A.ArrayTy(s),d) = (indent d; say "ArrayTy("; type_id( s);
			                     say ")")

  and type_id (A.Sub a) = (say"(";say a;say ")")
  and printExp (d,l) = (say"Program[";dolist d exp l;say"]")

  in  printExp(0,e0); sayln ""; TextIO.flushOut outstream

end
end