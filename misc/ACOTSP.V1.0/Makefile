
acotsp: acotsp.o TSP.o utilities.o ants.o InOut.o timer.o ls.o parse.o
	gcc -ansi acotsp.o TSP.o utilities.o ants.o InOut.o timer.o parse.o ls.o -o acotsp -g -lm 

acotsp.o: acotsp.c
	gcc -c acotsp.c -o acotsp.o -g -ansi -pedantic

TSP.o: TSP.c TSP.h
	gcc -c TSP.c -o TSP.o -g -ansi -pedantic

ants.o: ants.c ants.h
	gcc -c ants.c -o ants.o -g -ansi -pedantic

InOut.o: InOut.c InOut.h
	gcc -c InOut.c -o InOut.o  -g -ansi -pedantic

utilities.o: utilities.c utilities.h
	gcc -c utilities.c -o utilities.o  -g -ansi -pedantic

ls.o: ls.c ls.h
	gcc -c ls.c -o ls.o  -g -ansi -pedantic

parse.o: parse.c parse.h
	gcc -c parse.c -o parse.o  -g -ansi -pedantic

timer.o: timer.c timer.h
	gcc -c timer.c -o timer.o  -g -ansi -pedantic
