structure PT 
= struct


    structure T = Tree
    structure TEM = Temp 

    fun printRO T.EQ  = "EQ"
      | printRO T.NE  = "NE"
      | printRO T.LT  = "LT"
      | printRO T.GT  = "GT"
      | printRO T.GE  = "GE"
      | printRO T.LE  = "LE"
      | printRO T.ULT = "ULT"
      | printRO T.ULE = "ULE"
      | printRO T.EGT = "EGT"
      | printRO T.UGE = "UGE"
    
    fun printBO T.PLUS    = "+ "
      | printBO T.MINUS   = "- "
      | printBO T.MUL     = "* "
      | printBO T.DIV     = "/ "
      | printBO T.AND     = "& "
      | printBO T.OR      = "| "
      | printBO T.XOR     = "XOR "
      | printBO T.LSHIFT  = "<< "
      | printBO T.RSHIFT  = ">> "
      | printBO T.ARSHIFT = ">>> "


    fun printE (T.CONST i) = "\027[34m(CONST "^Int.toString(i)^")\027[39m"
      | printE (T.NAME  l) = "(NAME L" ^ Int.toString(l)^")"
      | printE (T.TEMP t)  = "(TEMP T" ^ Int.toString(t)^")"
      | printE (T.BINOP (opp, e1, e2)) = "\027[31mBINOP (" ^ (printBO opp)^ ","^(printE e1) ^"," ^(printE e2) ^")\027[39m"
      | printE (T.MEM (e)) = "( MEM "^(printE e) ^ ")"
      | printE (T.CALL (e, el)) = let fun printL [] = ""
                                        | printL [a] = (printE a)
                                        | printL (a::al) = (printE a) ^"," in      
                                  "CALL (" ^ (printE e) ^"," ^ (printL el) ^ ")"
                                  end
      | printE (T.ESEQ (s, e)) = "\027[33mESEQ(" ^ (printS s) ^ "," ^ (printE e) ^")\027[39m"

    and printS (T.MOVE (e1,e2)) = "\027[35mMOVE("^(printE e1)^","^(printE e2)^")\027[39m"
      | printS (T.EXP e) =  "EXP("^(printE e) ^")"
      | printS (T.JUMP (e,lt)) = let fun printL [] = ""
                                        | printL [a] = Int.toString(a)
                                        | printL (a::al) = Int.toString(a) ^"," in
                                "JUMP("^(printE e)^","^(printL lt) ^")"
                                end
      | printS (T.CJUMP (opp, e1, e2, l1,l2)) = "\027[36mCJUMP(" ^(printRO opp)^","^(printE e1) ^","^(printE e2)  ^"L"^Int.toString(l1)^", L"^Int.toString(l2) ^")\027[39m"
      | printS (T.SEQ (s1,s2)) = "\027[32mSEQ ("^(printS s1 )^","^(printS s2)^")\027[39m"
      | printS (T.LABEL l)     = "(LABEL L"^Int.toString(l)^")"

    and printT []        = ""
      | printT (e :: el) = (printE e) ^ "\n\n" ^ (printT el) 

end