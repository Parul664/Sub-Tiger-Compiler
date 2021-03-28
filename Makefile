TEST=tests
EXECUTABLE=compile
PRETTY_PRINTER=target/pp.sml
AST_PRINTER=tiger/printast.sml
MF = tiger

all : $(EXECUTABLE)

clean:
	rm -f ${MF}/*.lex.sml 
	rm -f ${MF}/*.grm.sml ${MF}/*.grm.desc ${MF}/*.grm.sig compile

%.lex.sml : %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<


$(EXECUTABLE) : ${MF}/compile.sml compile.mlb ${MF}/tiger.grm.sml ${MF}/tiger.lex.sml ${MF}/ast.sml ${AST_PRINTER} ${PRETTY_PRINTER}
	mlton compile.mlb

tests_pp : all
	./$(EXECUTABLE) $(TEST)/test1.tig --p
	./$(EXECUTABLE) $(TEST)/test2.tig --p
	./$(EXECUTABLE) $(TEST)/test3.tig --p
	./$(EXECUTABLE) $(TEST)/test4.tig --p

tests_ast : all
	./$(EXECUTABLE) $(TEST)/test1.tig --s
	./$(EXECUTABLE) $(TEST)/test2.tig --s
	./$(EXECUTABLE) $(TEST)/test3.tig --s
	./$(EXECUTABLE) $(TEST)/test4.tig --s

test3_ast : all
	./$(EXECUTABLE) $(TEST)/test3.tig --s
test3_p : all
	./$(EXECUTABLE) $(TEST)/test3.tig --p

test1_ast : all
	./$(EXECUTABLE) $(TEST)/test1.tig --s
test1_p : all
	./$(EXECUTABLE) $(TEST)/test1.tig --p

test2_ast : all
	./$(EXECUTABLE) $(TEST)/test2.tig --s
test2_p : all
	./$(EXECUTABLE) $(TEST)/test2.tig --p