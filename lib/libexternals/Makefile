all:
	ocamlopt -ccopt -fPIC -ccopt -Wall externals.c -shared -o externals.so
	ocamlopt -ccopt -Wall -c externals.c
	ar rv externals.a externals.o
	ranlib externals.a

clean:
	rm *.o *.a *.so
