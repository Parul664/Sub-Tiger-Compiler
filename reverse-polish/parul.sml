datatype BinOp = Plus
               | Minus
               | Mul
               | Div

datatype Expr  = Const of int
               | Op    of Expr * BinOp * Expr