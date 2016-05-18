
# TestU01 must be installed locally in the tu01 directory:
# Download TestU01 from http://simul.iro.umontreal.ca/testu01/TestU01.zip,
# unzip it, cd to its directory and configure with
#   ./configure --prefix=`pwd`/../tu01
# then "make" and "make install".

INCLUDE = -ccopt -I -ccopt tu01/include
LIBS = -cclib tu01/lib/libtestu01.a -cclib tu01/lib/libprobdist.a \
       -cclib tu01/lib/libmylib.a

testrng: testrng_stub.c random32mul.ml testrng.ml
	ocamlopt ${INCLUDE} $^ ${LIBS} -o testrng

clean:
	rm -f *.cm[aiotx] *.o testrng
