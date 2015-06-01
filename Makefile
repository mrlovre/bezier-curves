all:
	gcc -fPIC -shared matrix_inversion.c -o libmatrix_inversion.so
	ghc -lgsl -lgslcblas  --make Main libmatrix_inversion.so

run:
	@LD_LIBRARY_PATH=. ./Main
