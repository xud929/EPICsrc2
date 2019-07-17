target=soft_gaussian
OO=mathfunc.o faddeeva.o acc_model.o beam.o beambeam.o ${target}.o beamx.o 
${target}:../parser_input/check_input_compiled ${OO}
	mpic++ -o ${target} ${OO} ../parser_input/bb_lex.o ../parser_input/bb_yacc.o ../parser_input/calc_lex.o ../parser_input/calc_yacc.o ../parser_input/check_input.o
${target}.o:../beambeam.h ../beam.h ../acc_model.h ../faddeeva.h ../mathfunc.h ../${target}.cpp
	mpic++ -c -O3 ../${target}.cpp -std=c++11
mathfunc.o:../mathfunc.h ../mathfunc.cpp
	mpic++ -c -O3 ../mathfunc.cpp -std=c++11
faddeeva.o:../faddeeva.h ../faddeeva.cpp
	mpic++ -c -O3 ../faddeeva.cpp -std=c++11
acc_model.o:../acc_model.h ../mathfunc.h ../acc_model.cpp
	mpic++ -c -O3 ../acc_model.cpp -std=c++11
beam.o:../beam.h ../acc_model.h ../faddeeva.h ../mathfunc.h ../beam.cpp
	mpic++ -c -O3 ../beam.cpp -std=c++11
beambeam.o:../beambeam.h ../beam.h ../acc_model.h ../faddeeva.h ../mathfunc.h ../beambeam.cpp
	mpic++ -c -O3 ../beambeam.cpp -std=c++11
beamx.o:../beam.h ../beamx.cpp
	mpic++ -c -O3 ../beamx.cpp -std=c++11
../parser_input/check_input_compiled:
	cd ../parser_input && make
.PHONY: clean
clean:
	rm -f *.o main 
	cd ../parser_input && make clean
