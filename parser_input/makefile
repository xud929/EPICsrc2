check_input_compiled:bb_yacc.o bb_lex.o check_input.o calc_yacc.o calc_lex.o
	touch check_input_compiled
check_input.o:check_input.cpp
	g++ -c check_input.cpp -std=c++11
bb_lex.o:bb_lex.c
	gcc -c bb_lex.c
bb_yacc.o:bb_yacc.c
	g++ -c bb_yacc.c -std=c++11
bb_yacc.c:bb.y
	yacc -d bb.y -o bb_yacc.c
bb_lex.c:bb.l bb.y
	lex -o bb_lex.c bb.l
calc_lex.o:calc_lex.c
	gcc -c calc_lex.c
calc_yacc.o:calc_yacc.c
	g++ -c calc_yacc.c -std=c++11
calc_yacc.c:calc.y
	yacc -d calc.y -o calc_yacc.c
calc_lex.c:calc.l calc.y
	lex -o calc_lex.c -P zz calc.l
.PHONY:clean
clean:
	rm -f bb_lex.o bb_yacc.o bb_yacc.c bb_lex.c calc_lex.c calc_lex.o calc_yacc.c calc_yacc.o calc_yacc.h bb_yacc.h check_input_compiled check_input.o

