#This script compiles the fortran code and generates a python module

#For paralel with openmp.
f2py -c -lgomp --f90flags="-fopenmp -lgomp -g" -m w_motion_vectors w_motion_vectors_parallel.f90

#For single run without openmp.
#f2py -c -m motion_vectors motion_vectors_parallel.f90

