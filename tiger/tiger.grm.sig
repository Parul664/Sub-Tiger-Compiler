signature Expr_TOKENS =
sig
type ('a,'b) token
type svalue
val ID: (string) *  'a * 'a -> (svalue,'a) token
val EXP_MINUS:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val TO:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val SIN_Q:  'a * 'a -> (svalue,'a) token
val DUB_Q:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val LTE:  'a * 'a -> (svalue,'a) token
val GTE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val FUNCTION:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val ARRAY:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val RBRAC_R:  'a * 'a -> (svalue,'a) token
val LBRAC_R:  'a * 'a -> (svalue,'a) token
val RBRAC_SQ:  'a * 'a -> (svalue,'a) token
val LBRAC_SQ:  'a * 'a -> (svalue,'a) token
val RBRAC_C:  'a * 'a -> (svalue,'a) token
val LBRAC_C:  'a * 'a -> (svalue,'a) token
val NEWLINE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val CONST_STR: (string) *  'a * 'a -> (svalue,'a) token
val CONST_INT: (int) *  'a * 'a -> (svalue,'a) token
end
signature Expr_LRVALS=
sig
structure Tokens : Expr_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
