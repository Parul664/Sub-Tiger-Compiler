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
                  |  [x,y]  => makeFileLexer x
                  |  _      => (TextIO.output(TextIO.stdErr, "usage: ec file\n"); OS.Process.exit OS.Process.failure)


    fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					             "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

    val (program,_) = ExprParser.parse (0,thisLexer,print_error,()) 

    val args = case CommandLine.arguments() of
                  []  => PrintAbsyn.print(TextIO.stdOut,program)
                | [x] => PrintAbsyn.print(TextIO.stdOut,program)
                | [x,y] =>  ( case y of 
                                "--s" => PrintAbsyn.print(TextIO.stdOut,program)
                              | "--p" => TextIO.output(TextIO.stdOut,(PP.compile(program)))
                              | _     => (TextIO.output(TextIO.stdErr, "options \n --s => show AST \n --p => pretty print\n"); OS.Process.exit OS.Process.failure)
                            )
                | _    => (TextIO.output(TextIO.stdErr, "usage: ec file option \n option --s => show AST\n --p => pretty print\n"); OS.Process.exit OS.Process.failure)

    (* val _ = PrintAbsyn.print(TextIO.stdOut,program) *)

    (* val pretty_printed = let 
                            val temp = PP.compile(program)
                        in TextIO.output(TextIO.stdOut,temp)
                        end *)


end


