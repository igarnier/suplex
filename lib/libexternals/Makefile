
all:
	gcc -fPIC -Wall externals.c -shared -o externals.so
	gcc -Wall -c externals.c
	ar rv externals.a externals.o
	ranlib externals.a

clean:
	rm *.o *.a *.so
