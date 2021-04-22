TEST=tests
EXECUTABLE=tc
PRETTY_PRINTER=target/pp.sml
AST_PRINTER=tiger/printast.sml
MF = tiger

all : $(EXECUTABLE)

clean:
	rm -f ${MF}/*.lex.sml 
	rm -f ${MF}/*.grm.sml ${MF}/*.grm.desc ${MF}/*.grm.sig tc

%.lex.sml : %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<


$(EXECUTABLE) : tc.sml tc.mlb ${MF}/tiger.grm.sml ${MF}/tiger.lex.sml ${MF}/ast.sml ${AST_PRINTER} ${PRETTY_PRINTER}
	mlton tc.mlb

tests_pp : all
	./$(EXECUTABLE) $(TEST)/test1.tig --pp
	./$(EXECUTABLE) $(TEST)/test2.tig --pp
	./$(EXECUTABLE) $(TEST)/test3.tig --pp
	./$(EXECUTABLE) $(TEST)/test4.tig --pp

tests_ast : all
	./$(EXECUTABLE) $(TEST)/test1.tig --ast
	./$(EXECUTABLE) $(TEST)/test2.tig --ast
	./$(EXECUTABLE) $(TEST)/test3.tig --ast
	./$(EXECUTABLE) $(TEST)/test4.tig --ast

test3_ast : all
	./$(EXECUTABLE) $(TEST)/test3.tig --ast
test3_p : all
	./$(EXECUTABLE) $(TEST)/test3.tig --pp

test1_ast : all
	./$(EXECUTABLE) $(TEST)/test1.tig --ast
test1_p : all
	./$(EXECUTABLE) $(TEST)/test1.tig --pp

test2_ast : all
	./$(EXECUTABLE) $(TEST)/test2.tig --ast
test2_p : all
	./$(EXECUTABLE) $(TEST)/test2.tig --pp