cd ../src && sh compile.sh && cd -
gfortran -g -I../src -c main.f90
gfortran -g -I../src -o main.x main.o ../src/m_random.o
