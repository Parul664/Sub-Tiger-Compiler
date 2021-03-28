structure PP : sig
    val compile : Tiger.exp list -> string
end
= struct

   (* This function aligns the text by d spaces *)
    fun align 0 = ""
      | align d = "    " ^ (align (d-1))

    (* The tiger structure A, that contains the ast *)
    structure A = Tiger

    (* for pretty printing of operators *)
    fun binop A.Plus           = "+"
      | binop A.Minus          = "-"
      | binop A.Mul            = "*"
      | binop A.Div            = "/"
      | binop A.Equals         = "="
      | binop A.AngBrac        = "<>"
      | binop A.GreatThan      = ">"
      | binop A.LessThan       = "<"
      | binop A.GreatEqualThan = ">="
      | binop A.LessEqualThan  = "<="
      | binop A.And            = "&"
      | binop A.Or             = "|" 

    (* Color Scheme  
    Nil = Blue \027[34m
    if else = green \027[32m
    while = red \027[31m
    for = purple \027[35m
    let = light blue \027[36m
    decs = yellow \027[33m 
    ty = blue \027[34m
    *)

    (* Instead of the characters \n, a new line is printed on the screen, so in order to curb that problem
    this function is helpful
    - This function will replace all the \n as \\n, same for \t
    - When there are characters like \\n, \\n in the code, the they would be replaced by \\\\, so that \\n
    are printed litrally on the screen
     *)
    fun formathelp []  = []
      | formathelp (#"\n" :: xs ) = ( "\\n" :: (formathelp xs) )
      | formathelp (#"\t" :: xs ) = ("\\t" :: (formathelp xs))
      | formathelp ((#"\\") :: xs ) = (( "\\\\" :: formathelp xs))
      | formathelp (x :: xs) = ((String.str x) :: (formathelp xs) )

    (* Splits the string into list of characters to find out special characters from it *)
    fun formatstr s =  formathelp ( String.explode s )
    fun format [] = "" 
      | format (s :: st) = s ^ (format st)

    (* ******************************************************************************* *)
    (* This function indents the expression exp *)
    (* The expressions like nil, integer, string, negation of exp, BinOp will not be self aligned with respect to the left border
    It would be the responsibility of the caller to align them 
    *)
    fun indent A.NilExpr d = "\027[34mnil \027[39m"
      | indent (A.SExpr s) d = "\"" ^ format(formatstr s) ^ "\" "
      | indent (A.IExpr i) d = Int.toString(i) ^ " "
      | indent (A.ArrayExpr {Type, Sizee, Initial}) d =
            (tyid Type) ^ " [ " ^ (indent Sizee d) ^ " ] of " ^ (indent Initial d)

      | indent (A.RecordExpr {Type,Values}) d =
            let
                (* function for indenting the fields inside the Record *)
                (* temp1 indents A single id,exp pair 
                   temp2 uses temp1 to indent a list of id,exp pairs
                *)
                fun temp1 i ex  = (i ^ "=" ^(indent ex d)) 
                fun temp2 []             = ""
                  | temp2 ([(i,ex)])        = (temp1 i ex)
                  | temp2 ( (i,ex) :: ls) = (temp1 i ex) ^", "^ (temp2 ls) 
            in 
                (tyid Type) ^ " { " ^ (temp2 Values) ^ " } " 
            end
      
      | indent (A.LValueExpr l) d = (indentlv l d)

      | indent (A.FuncCallExpr {Id, Param}) d = 
            let 
                (* functions for indenting the parameters of a function *)
                fun temp []          = ""
                  | temp [e1]        = (indent e1 d)
                  | temp ( e1 :: e ) = (indent e1 d) ^ ", " ^ (temp e) 
            in 
                Id ^ " ( " ^ (temp Param) ^ ") "
            end
      | indent (A.NegationExpr a) d    = "-" ^ (indent a d)
      | indent (A.BinOpExpr (e1,bop,e2)) d = (indent e1 d) ^ (binop bop) ^ (indent e2 d)

      (* This is self indenting exp, meaning it will start itself from a new line and then align as per given d *)
      | indent (A.SeqExpr el) d = 
            let 
                (* While printing seqExpr, we first check the the exp inside the SeqExpr is a self indenting 
                  expr or not, in case it is self indenting, then we do not indent it, else we indent it.
                  Function temp prints the expr one by one
                 *)
                fun temp []          = "\n"
                  | temp [e1]        = (case (ifteller e1) of 
                                         1 => ""
                                         | _ => "\n"^(align (d+1))
                                         )^ (indent e1 (d+1)) ^ "\n"
                  | temp ( e1 :: e ) = (case (ifteller e1) of 
                                         1 => ""
                                         | _ => "\n"^(align (d+1))
                                         ) ^ (indent e1 (d+1)) ^ ";"^ (temp e )              in 
                "\n"^(align d) ^ "(" ^ (temp el) ^ (align d) ^ " ) "
            end
      
      | indent (A.AssignExpr(lv,e)) d = (indentlv lv d) ^":="^ (indent e d)

      (* starts always from a newline. All the functions below start with a newline, except break*)
      | indent (A.IfExpr {Cond,Do, Else}) d = 
            "\n" ^ (align d) ^ "\027[32mif\027[39m " ^ (indent Cond (d+1)) ^ 
            "\n" ^ (align d) ^ "\027[32mthen\027[39m " ^ 
            (teller (ifteller Do) Do d) ^
             
            (case Else of NONE => ""
                | SOME(s) => "\n" ^ (align d) ^ "\027[32melse\027[39m " ^ (teller (ifteller s) s d)) ^ " "
      
      | indent( A.WhileExpr {Cond, Do}) d = 
            "\n" ^ (align d) ^ "\027[31mwhile\027[39m " ^(indent Cond (d+1)) ^
            "\n" ^ (align d) ^ "\027[31mdo\027[39m " ^ 
            (teller (ifteller Do) Do d) ^ " "
      
      | indent (A.ForExpr {Id, StartPos, EndPos,Do}) d = 
            "\n" ^ (align d) ^ "\027[35mfor\027[39m " ^ Id ^ ":=" ^ (indent StartPos (d+1)) ^ "\027[35mto\027[39m " ^
            (indent EndPos (d+1)) ^ "\027[35mdo\027[39m " ^
            (teller (ifteller Do) Do d) ^ " "

      | indent A.BreakExpr d=  "\027[31mbreak\027[39m "
      | indent (A.LetExpr (dl, el)) d= 
            (* temp for dec list indentation and temp2 for exp list indentation
              I made all the declarations self indenting, directly we call printing for dec
             *)
            let 
            
                fun temp [] = "\n"
                  | temp (dd :: dl) = (indentdec dd (d+1) ) ^ (temp dl)
                fun temp2 []          = "\n"
                  | temp2 [e1]        = (case (ifteller e1) of 
                                         1 => ""
                                         | _ => "\n"^(align (d+1))
                                         )^ (indent e1 (d+1)) ^ "\n"
                  | temp2 ( e1 :: e ) = (case (ifteller e1) of 
                                         1 => ""
                                         | _ => "\n"^(align (d+1))
                                         ) ^ (indent e1 (d+1)) ^ ";"^ (temp2 e ) 
            in 
            "\n"^(align d) ^ "\027[36mlet\027[39m " ^ (temp dl) ^ (align d) ^ "\027[36min\027[39m " ^ (temp2 el) ^(align d) ^"\027[36mend\027[39m "
            end


(* *********************DEC PRETTY PRINTING************************** *)

    and indentdec (A.TypeDec (id, ty)) d= "\n"^(align d)^"\027[33mtype\027[39m " ^ id ^ "= " ^ (indentty ty)
      | indentdec( A.VarDec (id, bop, e)) d = 
                "\n"^(align d)^"\027[33mvar\027[39m " ^ id ^ (case bop of
                NONE      => ""
                | SOME(s) => ":" ^ (tyid s) 
                ) ^ ":=" ^ (indent e (d+1)) ^ " "
      | indentdec (A.FuncDecs(A.fundec{Id,Param,RetType,Body})) d=
                let
                    (* temp1 and temp2 for list of id, typeid list *)
                    fun temp1 i t  = i ^ ":" ^(tyid t) 
                    fun temp2 []             = ""
                      | temp2 ([(i,e)])        = (temp1 i e)
                      | temp2 ( (i,e) :: ls) = (temp1 i e) ^", "^ (temp2 ls) 
            
                in
                    "\n"^(align d)^"\027[33mfunction\027[39m "^Id ^"(" ^ (temp2 (tyf Param)) ^ ")"
                    ^ (case RetType of SOME(s) => ": " ^ (tyid s)
                        | NONE => ""
                    ) ^ " = " ^ (indent Body (d+1)) ^ " "
                end


  (* *****************TY PRETTY PRINTING******************* *)

    and indentty (A.AlreadyTy a) = (tyid a) ^ " " 
      | indentty (A.SeqTy (A.TF s))  = 
            let
                (* temp1 and temp2 are used for printing a sequence of types *)
                fun temp1 i t  = i ^ ":" ^(tyid t) 
                fun temp2 []             = ""
                  | temp2 ([(i,e)])        = (temp1 i e)
                  | temp2 ( (i,e) :: ls) = (temp1 i e) ^", "^ (temp2 ls) 
            in "{" ^(temp2 s)^ "} "
            end
      | indentty (A.ArrayTy t) = "\027[34marray of\027[39m " ^ (tyid t) ^ " "

  (* ********************SIMPLE VARIABLES********************* *)

    and indentlv (A.SimpleVar id) d     = id  
      | indentlv (A.FeildVar (l ,id)) d = (indentlv l d) ^ "."^ id 
      | indentlv (A.ArrEl (l,e)) d      = (indentlv l d) ^ "["^ (indent e (d+1))^"] "

  (* *******************IDENTATION OVER************************ *)

  (* This function tells us whether the expr is self indenting or not.
  Helps to decide whether we should again indent it or not *)
    and ifteller (A.IfExpr{Cond,Do,Else}) = 1
      | ifteller (A.WhileExpr {Cond,Do})  = 1
      | ifteller (A.ForExpr {Id, StartPos,EndPos,Do}) =1
      | ifteller (A.LetExpr (d,e))        = 1
      | ifteller (A.SeqExpr(e))           = 1
      | ifteller _                        = 0


    (* this function indents the expr in case it is not self indenting, the first parameter is the 
    output of ifteller *)
    and teller 0 e d = "\n" ^ (align (d+1)) ^(indent e (d+1)) 
      | teller _ e d = (indent e (d+1))
    
    (* TypeID is inside the Sub construct, so this function removes that construct *)
    and tyid (A.Sub s) = s

    (* Same case as above for tyfields *)
    and tyf (A.TF s) = s


    (* This is the main function, that takes a list of expressions and then
    indents each expression *)
    fun compile []         = "\n"
      | compile (e1 :: e2) = (indent e1 0) ^ 
                             (case (ifteller e1) of
                                0 => "\n" 
                              | _ => ""
                             ) ^ (compile e2) 

end