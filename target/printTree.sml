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

    fun align 0 = ""
      | align d = "    " ^ align (d-1)


    fun printE d (T.CONST i) = "\027[34m(CONST \027[39m"^Int.toString(i)^"\027[36m)\027[39m"
      | printE d (T.NAME  l) = "(NAME L" ^ Int.toString(l)^")"
      | printE d (T.TEMP t)  = "(TEMP T" ^ Int.toString(t)^")"
      | printE d (T.BINOP (opp, e1, e2)) = "\027[31mBINOP (\027[39m" ^ (printBO opp)^ ","^(printE d e1) ^"," ^(printE d e2) ^"\027[31m)\027[39m"
      | printE d (T.MEM (e)) = "( MEM "^(printE d e) ^ ")"
      | printE d (T.CALL (e, el)) = let fun printL [] = ""
                                        | printL [a] = (printE d a)
                                        | printL (a::al) = (printE d a) ^"," in      
                                  "CALL (" ^ (printE d e) ^"," ^ (printL el) ^ ")"
                                  end
      | printE d (T.ESEQ (s, e)) = "\027[33mESEQ(\027[39m\n" ^(align (d+1))^ (printS (d+1) s) ^ ",\n"^ (align (d+1)) ^ (printE (d+1) e) ^"\027[33m)\027[39m"

    and printS d (T.MOVE (e1,e2)) = "\027[35mMOVE(\027[39m"^(printE d e1)^"\027[35m,\027[39m"^(printE d e2)^"\027[35m)\027[39m"
      | printS d (T.EXP e) =  "EXP("^(printE d e) ^")"
      | printS d (T.JUMP (e,lt)) = let fun printL [] = ""
                                        | printL [a] = Int.toString(a)
                                        | printL (a::al) = Int.toString(a) ^"," in
                                "JUMP("^(printE d e)^","^(printL lt) ^")"
                                end
      | printS d (T.CJUMP (opp, e1, e2, l1,l2)) = "\027[36mCJUMP(\027[39m" ^(printRO opp)^"\027[36m,\027[39m"^(printE d e1) ^
                                                  "\027[36m,\027[39m"^(printE d e2)  ^"L"^Int.toString(l1)^", L"^Int.toString(l2) ^"\027[36m)\027[39m"
      | printS d (T.SEQ (s1,s2)) = "\027[32mSEQ (\n\027[39m"^(align (d+1))^(printS (d+1) s1 )
                                    ^"\027[32m,\n\027[39m"^(align (d+1))^(printS (d+1) s2)^"\027[32m)\027[39m"
      | printS d (T.LABEL l)     = "(LABEL L"^Int.toString(l)^")"

    and printT s = (printS 0 s) ^ "\n\n"  

    (* function for printing the canonised version *)
    fun printC []      = "\n"
      | printC (l::ls) = (printS 0 l)^ "\n" ^ (printC ls)

end