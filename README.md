
# Description

Derived type to hold a random number generator, which does not interfere with the
global `RANDOM_NUMBER()` routine of fortran.

Using `t_random` can be useful when coupling software which depend on specifically
seeded random number generator for internal use, and should not clash with another.

# Compile

Straightforward compile command:

```bash
gfortran -c m_random.f90
```

# Caller program

Link the caller program with `m_random.o` object by specifying the include path `-I` to `src`. See also the `test/compile.sh` script.

Example program:

```f90
program main

  use m_random, only: t_random
  implicit none
  
  type( t_random ), pointer :: rd1, rd2
  real :: z
  real :: zv(6)
  
  ! initialize with specific seed
  rd1 => t_random( 1234 )
  
  ! initialize another type
  rd2 => t_random( 5678 )
  
  ! generate numbers
  call rd1% random_number( z )
  
  ! or vectors
  call rd2% random_number( zv )
  
  ! re-seed to another value
  call rd1% reseed( 6543 )
  
  ! finalize
  deallocate( rd1 )
  deallocate( rd2 )
end program main
```
