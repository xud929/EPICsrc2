test:calc.l calc.y
	yacc -d calc.y -o calc_yacc.c
	lex -o calc_lex.c calc.l 
	gcc -c calc_lex.c
	g++ -c calc_yacc.c -std=c++11
	g++ -c test.cpp -std=c++11
	g++ -o test test.o calc_lex.o calc_yacc.o
.PHONY:clean
clean:
	rm -f test test.o lex.zz.o y.tab.o lex.zz.c y.tab.c calc*.c calc*.o calc*.o y.tab.h
