(* Reference - Sir's Reverse Polish Compiler *)

structure TIGER = struct

    structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token)
    structure ExprLex    = ExprLexFun(structure Tokens = ExprLrVals.Tokens)
    structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
                                 structure Lex        = ExprLex
                                 structure LrParser   = LrParser
                                )

    fun makeExprLexer strm = ExprParser.makeLexer(fn n => TextIO.inputN(strm,n))
    val makeFileLexer      = makeExprLexer o TextIO.openIn

    val thisLexer = case CommandLine.arguments() of
                     []  => makeExprLexer TextIO.stdIn
                  |  [x] => makeFileLexer x
                  |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)



    fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					             "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

    val (program,_) = ExprParser.parse (0,thisLexer,print_error,()) 

end
