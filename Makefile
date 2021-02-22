
all : tc

tc : src/tc.sml
	mlton $< 
	mv src/tc tc

clean:
	rm -f tc
