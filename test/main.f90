program main

  use m_random, only: t_random
  implicit none

  type( t_random ), pointer :: rr

  real :: z, r(5)
  integer :: i, n, zseed
  integer, allocatable :: orig_seed(:)

  write(*,*)
  write(*,*) repeat("=",60)
  write(*,*) " This test is to show that calling t_random%random_number() does not interfere"
  write(*,*) " with the internal state of the global RANDOM_NUMBER() generator."
  write(*,*) repeat("=",60)
  write(*,*)

  call random_seed(size=n)
  allocate(orig_seed(1:n))
  call random_seed(get=orig_seed)

  write(*,*) repeat('-',20)
  write(*,*) "create 10 reference values for the global RANDOM_NUMBER():"
  write(*,*) repeat('-',20)
  write(*,*) "call global random_number 10 times:"
  do i = 1, 10
     call random_number(z)
     write(*,*) i, z
  end do
  write(*,*)

  write(*,*) repeat('-',20)
  write(*,*) "create 10 reference values for t_random%random_number():"
  write(*,*) repeat('-',20)

  rr => t_random()
  write(*,*) "create t_random with zseed:", rr%zseed, "and call t_random%random_number() 10 times:"
  do i = 1, 10
     call rr% random_number(z)
     write(*,*) i, z
  end do
  write(*,*)


  write(*,*) repeat('-',20)
  write(*,*) "Reset both generators to initial states. "
  write(*,*) "Call the global, and t_random in a mixed order. The values produced should"
  write(*,*) "exactly match the reference values above."
  write(*,*) repeat('-',20)

  write(*,*) "reset global random_number to initial state, call it 5 times"
  call random_seed(put=orig_seed)
  do i = 1, 5
     call random_number(z)
     write(*,*) i, z
  end do
  write(*,*)

  write(*,*) "reset t_random to initial state, call it 5 times"
  call rr%reseed( rr%zseed )
  do i = 1, 5
     call rr%random_number(z)
     write(*,*) i, z
  end do
  write(*,*)

  write(*,*) "call global random_number() 5 more times"
  do i = 6,10
     call random_number(z)
     write(*,*) i, z
  end do
  write(*,*)


  write(*,*) "call t_random 5 more times (now with a vector)"
  call rr%random_number(r)
  do i = 1, size(r)
     write(*,*) i+5, r(i)
  end do

  deallocate(rr)

  deallocate( orig_seed )
end program main
