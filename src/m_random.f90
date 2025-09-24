module m_random

  use, intrinsic :: iso_fortran_env, only: real32, real64, real128
  implicit none
  private
  public :: t_random


  !> Derived type to hold a random number generator, which does not interfere with the
  !! global `RANDOM_NUMBER()` routine of fortran.
  !!
  !! Using `t_random` can be useful when coupling software which depend on specifically
  !! seeded random number generator for internal use, and should not clash with another.
  !!
  type t_random
     integer, public :: zseed !! value of `zseed` at which this instance of `t_random` is initialized.
     integer, allocatable, private :: my_state(:), other_state(:)
   contains
     procedure, public :: reseed => t_random_reseed

     procedure, private :: t_random_number_real32
     procedure, private :: t_random_number_real64
     procedure, private :: t_random_number_real128
     generic, public :: random_number => t_random_number_real32
     generic, public :: random_number => t_random_number_real64
     generic, public :: random_number => t_random_number_real128
     final :: t_random_destroy
  end type t_random

  interface t_random
     procedure t_random_initialize
  end interface t_random

contains


  function t_random_initialize( zseed )result(self)
    !! Create the `t_random` object with a seed value `zseed`. If `zseed=0`, a new seed is generated
    !! based on `system_clock` values.
    !!
    !! Call as:
    !!
    !!```f90
    !! type( t_random ), pointer :: rnd
    !! integer :: zseed
    !!
    !! ! with specified value for zseed:
    !! rnd => t_random( zseed )
    !!
    !! ! or without zseed (this is equivalent to zseed=0)
    !! rnd => t_random()
    !!```
    implicit none
    integer, intent(in), optional :: zseed !! if not present, new seed is generated
    type( t_random ), pointer :: self
    integer :: zs
    zs = 0
    if(present(zseed))zs=zseed
    allocate(t_random::self)
    call t_random_reseed( self, zs )
  end function t_random_initialize


  subroutine t_random_destroy( self )
    implicit none
    type( t_random ), intent(inout) :: self
    if( allocated(self%my_state)   ) deallocate( self%my_state )
    if( allocated(self%other_state)) deallocate( self%other_state )
  end subroutine t_random_destroy

  subroutine t_random_number_real32( self, z )
    !! Obtain a random real number `z`, on range [0:1].
    implicit none
    class( t_random ), intent(inout) :: self
    real(real32), intent(out) :: z(..) !! random[0:1], up to rank-3
    ! save external state
    call random_seed(get=self%other_state)
    ! put my state
    call random_seed(put=self%my_state)
    ! generate number
    select rank(z)
    rank(0); call random_number(z)
    rank(1); call random_number(z)
    rank(2); call random_number(z)
    rank(3); call random_number(z)
    end select
    ! save my new state
    call random_seed(get=self%my_state)
    ! re-put old external state
    call random_seed(put=self%other_state)
  end subroutine t_random_number_real32
  subroutine t_random_number_real64( self, z )
    !! Obtain a random real number `z`, on range [0:1].
    implicit none
    class( t_random ), intent(inout) :: self
    real(real64), intent(out) :: z(..) !! random[0:1], up to rank-3
    ! save external state
    call random_seed(get=self%other_state)
    ! put my state
    call random_seed(put=self%my_state)
    ! generate number
    select rank(z)
    rank(0); call random_number(z)
    rank(1); call random_number(z)
    rank(2); call random_number(z)
    rank(3); call random_number(z)
    end select
    ! save my new state
    call random_seed(get=self%my_state)
    ! re-put old external state
    call random_seed(put=self%other_state)
  end subroutine t_random_number_real64
  subroutine t_random_number_real128( self, z )
    !! Obtain a random real number `z`, on range [0:1].
    implicit none
    class( t_random ), intent(inout) :: self
    real(real128), intent(out) :: z(..) !! random[0:1], up to rank-3
    ! save external state
    call random_seed(get=self%other_state)
    ! put my state
    call random_seed(put=self%my_state)
    ! generate number
    select rank(z)
    rank(0); call random_number(z)
    rank(1); call random_number(z)
    rank(2); call random_number(z)
    rank(3); call random_number(z)
    end select
    ! save my new state
    call random_seed(get=self%my_state)
    ! re-put old external state
    call random_seed(put=self%other_state)
  end subroutine t_random_number_real128


  subroutine t_random_reseed( self, zseed )
    !! re-seed the `t_random` object with a specific value of `zseed`.
    !!
    !! initialize random number generator, modified from:
    !! https://gcc.gnu.org/onlinedocs/gcc-4.9.1/gfortran/RANDOM_005fSEED.html
    !!
    !! Seed the random number generator with sequence generated from zseed.
    !! If on input `zseed = 0` then a new, repeatable seed sequence is generated.
    !! On output, `zseed` has value of actual seed used (single value), which can be
    !! used to reproduce the actual sequence of seed elements.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    class( t_random ), intent(inout) :: self
    integer, intent(inout) :: zseed
    integer :: n, i
    integer(int64) :: t

    if( zseed == 0 ) then
       ! generate seed from system clock
       call system_clock(t)
       t = mod( t, int(huge(0), int64) )
       zseed = int( t )
    end if
    ! save the generated zseed
    self%zseed = zseed

    call random_seed(size=n)
    if( allocated(self%other_state) )then
       if( size(self%other_state) /= n )deallocate(self%other_state)
    end if
    if( allocated(self%my_state) )then
       if( size(self%my_state) /= n)deallocate(self%my_state)
    end if
    if( .not.allocated(self%other_state))allocate(self%other_state(1:n))
    if( .not.allocated(self%my_state   ))allocate(self%my_state(1:n))

    ! put first element of seed
    self%my_state(1) = lcg( int(zseed, int64) )
    do i = 2, n
       ! other elements of seed are function of preceding seed element
       self%my_state(i) = lcg( int(self%my_state(i-1), int64) )
    end do

  contains
    ! This simple PRNG might not be good enough for real work, but is
    ! sufficient for seeding a better PRNG.
    function lcg(s)
      integer :: lcg
      integer(int64) :: s
      if (s == 0) then
         s = 104729
      else
         s = mod(s, 4294967296_int64)
      end if
      s = mod(s * 279470273_int64, 4294967291_int64)
      lcg = int(mod(s, int(huge(0), int64)), kind(0))
    end function lcg

  end subroutine t_random_reseed


end module m_random
