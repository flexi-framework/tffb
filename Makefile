COMPILER_FORTRAN=gfortran
FORTRAN_FLAGS=-fPIC
COMPILER_CPP=g++
CPP_FLAGS=-fPIC
LINKER=gcc
LINKER_FLAGS=-fPIC
LIB_NAME=libtffb

all: libtffb run
	@mkdir -p build
	@mv test ${LIB_NAME}.* *.log *.err build
	@rm -f ${LIB_NAME}.* *.o *.mod *.log *.err

libtffb: tf_c.o tf_f.o
	@if [ x"${TF_DIR}" = "x" ]; then \
	  echo "\e[31mNo TensorFlow directory is set! Please set the correct directory with 'export TF_DIR=/Path/To/TF_DIR'.\e[0m\n"; \
	  exit 1; \
	else \
	  echo "Searching for TensorFlow directory at '${TF_DIR}'\n"; \
	fi
	ar rvs ${LIB_NAME}.a tf_c.o tf_f.o
	${LINKER} ${LINKER_FLAGS} -shared -o ${LIB_NAME}.so  tf_f.o tf_c.o -L${TF_DIR}/lib -ltensorflow -ltensorflow_framework

tf_c.o: src/tf.cpp
	@if [ x"${TF_DIR}" = "x" ]; then \
	  echo "\e[31mNo TensorFlow directory is set! Please set the correct directory with 'export TF_DIR=/Path/To/TF_DIR'.\e[0m\n"; \
	  exit 1; \
	else \
	  echo "Searching for TensorFlow directory at '${TF_DIR}'\n"; \
	fi
	${COMPILER_CPP} ${CPP_FLAGS} -c src/tf.cpp -o tf_c.o -I${TF_DIR}/include

tf_f.o: src/tf.f90
	${COMPILER_FORTRAN} ${FORTRAN_FLAGS} -g -c src/tf.f90 -o tf_f.o

run: test
	@ echo 'Running short test...'
	./test > test.log 2> test.err
	@ echo 'Successfull!'

test: libtffb test.o
	${COMPILER_FORTRAN} test.o -o test -L/home/iagmkurz/Code/flexi/flexi/share/GNU/tensorflow/lib/ -L. -ltensorflow_framework -ltensorflow -ltffb

test.o:
	${COMPILER_FORTRAN} ${FORTRAN_FLAGS} -g -c tests/test.f90

build_dir:
	mkdir -p build

.PHONY : clean
clean:
	rm -f ${LIB_NAME}.* *.o *.mod test *.log *.err
