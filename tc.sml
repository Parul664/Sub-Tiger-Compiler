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

    (* This value is 1 if we have to pretty print, it is 2 for ast, and 0 if nothing to be done *)
    val teller : int ref = ref 0
    val thisLexer = case CommandLine.arguments() of
                     []  => makeExprLexer TextIO.stdIn
                  |  [x] => makeFileLexer x
                  |  [x,y]  => (case x of "--pp" => (teller := 1;makeFileLexer y )
                                        | "--ast" => (teller :=2;makeFileLexer y)
                                        | "--t"  => (teller :=3 ; makeFileLexer y)
                                        | "--c" => (teller := 4; makeFileLexer y)
                                        | _     => (case y of "--pp" => (teller := 1;makeFileLexer x)
                                                            | "--ast" => (teller := 2;makeFileLexer x)
                                                            | "--t"  => (teller :=3 ; makeFileLexer x)
                                                            | "--c" => (teller := 4; makeFileLexer x)
                                                            | _     => (TextIO.output(TextIO.stdErr, "options \n --ast => show AST \n --pp => pretty print\n --t Tree IR\n --c Canonised Tree IR\n"); OS.Process.exit OS.Process.failure)
                                                    )
                                )
                  |  _      => (TextIO.output(TextIO.stdErr, "usage: tc [options] file [options]\n"); OS.Process.exit OS.Process.failure)


    fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					             "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

    val (program,_) = ExprParser.parse (0,thisLexer,print_error,()) 

    val args = case CommandLine.arguments() of
                  []  => PrintAbsyn.print(TextIO.stdOut,program)
                | [x] => PrintAbsyn.print(TextIO.stdOut,program)
                | [x,y] =>  ( case !teller of 
                                4 => (TextIO.output(TextIO.stdOut,(PT.printC (Canon.linearize(Translate.transList(program))))))
                              | 3 => (TextIO.output(TextIO.stdOut,(PT.printT (Translate.transList(program)))))
                              | 2 => PrintAbsyn.print(TextIO.stdOut,program)
                              | 1 => TextIO.output(TextIO.stdOut,(PP.compile(program)))
                              | _     => (TextIO.output(TextIO.stdErr, "options \n --ast => show AST \n --pp => pretty print\n --t Tree IR\n --c Canonised Tree IR\n"); OS.Process.exit OS.Process.failure)
                            )
                | _    => (TextIO.output(TextIO.stdErr, "usage: ec file option \n option --ast => show AST\n --pp => pretty print\n --t Tree IR\n --c Canonised Tree IR\n"); OS.Process.exit OS.Process.failure)

    (* val _ = PrintAbsyn.print(TextIO.stdOut,program) *)

    (* val pretty_printed = let 
                            val temp = PP.compile(program)
                        in TextIO.output(TextIO.stdOut,temp)
                        end *)


end


