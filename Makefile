machina:	machina.tab.c machina.lex.c
	gcc -o machina machina.tab.c lex.yy.c
machina.tab.c:	machina.y
	bison -dv machina.y
machina.lex.c:	machina.l
	flex machina.l
clean:
	rm  machina.tab.c machina.tab.h machina.output lex.yy.c machina
