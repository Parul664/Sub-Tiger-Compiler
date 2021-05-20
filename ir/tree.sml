
structure Temp:TEMP =
struct
    type label = int
    type temp = int

    val nextLabel : label ref= ref 0
    val nextTemp :  temp ref = ref 4

    fun newlabel () = (nextLabel := !nextLabel + 1; !nextLabel-1)
    fun newtemp ()  = (nextTemp := !nextTemp+ 1; !nextTemp-1 )
 
end

structure Tree =
struct  
    structure Temp = Temp
    datatype exp = CONST of int
                    (* label in the machine language *)
                 | NAME of Temp.label
                 (* register in the machine language *)
                 | TEMP of Temp.temp
                 | BINOP of binop * exp * exp
                 (* The contents of WordSize bits starting at address exp *)
                 | MEM of exp
                 | CALL of exp * exp list
                 | ESEQ of stm * exp

    and stm = MOVE of exp * exp
            | EXP of exp
            | JUMP of exp * Temp.label list
            | CJUMP of relop * exp * exp * Temp.label * Temp.label
            | SEQ of stm * stm
            | LABEL of Temp.label

    (* ARSHIFT treats the number as signed and shift it
    LSHIFTS and RSHIFTS are Logical shift, treating numbers as just string of bits    
     *)
    and binop = PLUS | MINUS | MUL | DIV | AND 
              | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

    and relop = EQ | NE | LT | GT | LE | GE 
              | ULT | ULE | EGT | UGE

end