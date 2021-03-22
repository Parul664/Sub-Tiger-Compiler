TEST=tests
EXECUTABLE=compile

clean:
	rm -f *.lex.sml rp
	rm -f *.grm.sml *.grm.desc *.grm.sig compile

%.lex.sml : %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<

all : $(EXECUTABLE)


$(EXECUTABLE) : compile.sml compile.mlb tiger.grm.sml tiger.lex.sml ast.sml printast.sml
	mlton compile.mlb

tests : all
	./$(EXECUTABLE) $(TEST)/test1.tig
	./$(EXECUTABLE) $(TEST)/test2.tig
	./$(EXECUTABLE) $(TEST)/test3.tig

test3 : all
	./$(EXECUTABLE) $(TEST)/test3.tig

test1 : all
	./$(EXECUTABLE) $(TEST)/test1.tig

test2 : all
	./$(EXECUTABLE) $(TEST)/test2.tig