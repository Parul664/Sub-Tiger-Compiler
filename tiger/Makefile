TEST=tests
EXECUTABLE=compile

clean:
	rm -f *.lex.sml rp
	rm -f *.grm.sml *.grm.desc *.grm.sig $(EXECUTABLE)

%.lex.sml : %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<

all : $(EXECUTABLE)


$(EXECUTABLE) : compile.sml compile.mlb tiger.grm.sml tiger.lex.sml ast.sml
	mlton compile.mlb

tests : all
	./$(EXECUTABLE) $(TEST)/test1.tig
	./$(EXECUTABLE) $(TEST)/test2.tig
	./$(EXECUTABLE) $(TEST)/test3.tig