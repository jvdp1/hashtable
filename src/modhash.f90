module modhash
 use iso_fortran_env, only: int32, real32, real64
 implicit none
 private
 public :: hash, roundinguppower2

 integer(int32), parameter, public :: seed_hash = 305419896_int32

 interface hash
  module procedure hashchar, hashint32, hashreal32, hashint32_scal, hashreal32_scal
 end interface

contains

 pure function hashchar(k, seed) result(c)
  character(len=*), intent(in) :: k
  integer(int32), intent(in), optional :: seed
  integer(int32) :: c

  integer(int32) :: modlength, lenk, length, i, j
  integer(int32), allocatable :: kint32(:)

  lenk = len_trim(k)
  modlength = int(lenk/4_int32)

  length = modlength
  if (mod(lenk, 4) .ne. 0) length = length + 1

  allocate (kint32(length))

  j = 1
  do i = 1, modlength*4, 4
   kint32(j) = transfer(k(i:min(i + 3, lenk)), kint32(i))
   j = j + 1
  end do

  select case (mod(lenk, 4))
  case (3)
   kint32(length) = transfer(k(modlength*4 + 1:lenk)//repeat(' ', 1), kint32(i))
  case (2)
   kint32(length) = transfer(k(modlength*4 + 1:lenk)//repeat(' ', 2), kint32(i))
  case (1)
   kint32(length) = transfer(k(modlength*4 + 1:lenk)//repeat(' ', 3), kint32(i))
  case default
  end select

  if (present(seed)) then
   c = hashint32(kint32, seed)
  else
   c = hashint32(kint32)
  end if
 end function

 pure function hashint32(k, seed) result(c)
  integer(kind=int32), intent(in) :: k(:)
  integer(kind=int32), intent(in), optional :: seed

  integer(kind=int32) :: length
  integer(kind=int32) :: a, b, c
  integer(kind=int32) :: pos

  integer(kind=int32) :: seed_

  seed_ = seed_hash
  if (present(seed)) seed_ = seed

  length = size(k)

  a = seed_ + ishft(length, 2)
  b = a
  c = a

  pos = 1
  do while (length > 3)
   a = a + k(pos)
   b = b + k(pos + 1)
   c = c + k(pos + 2)
   call mix(a, b, c)
   length = length - 3
   pos = pos + 3
  end do

  select case (length)
  case (1)
   a = a + k(pos)
  case (2)
   a = a + k(pos)
   b = b + k(pos + 1)
  case (3)
   a = a + k(pos)
   b = b + k(pos + 1)
   c = c + k(pos + 2)
  case default
  end select

  call final(a, b, c)

 end function

 pure function hashreal32(k, seed) result(c)
  real(kind=real32), intent(in) :: k(:)
  integer(kind=int32), intent(in), optional :: seed

  integer(kind=int32) :: length
  integer(kind=int32) :: a, b, c
  integer(kind=int32) :: pos

  integer(kind=int32) :: seed_

  seed_ = seed_hash
  if (present(seed)) seed_ = seed

  length = size(k)

  a = seed_ + ishft(length, 2)
  b = a
  c = a

  pos = 1
  do while (length > 3)
   a = a + transfer(k(pos), a)
   b = b + transfer(k(pos + 1), b)
   c = c + transfer(k(pos + 2), c)
   call mix(a, b, c)
   length = length - 3
   pos = pos + 3
  end do

  select case (length)
  case (1)
   a = a + transfer(k(pos), a)
  case (2)
   a = a + transfer(k(pos), a)
   b = b + transfer(k(pos + 1), b)
  case (3)
   a = a + transfer(k(pos), a)
   b = b + transfer(k(pos + 1), b)
   c = c + transfer(k(pos + 2), c)
  case default
  end select

  call final(a, b, c)

 end function

 pure function hashint32_scal(k, seed) result(c)
  integer(int32), intent(in) :: k
  integer(int32), intent(in), optional :: seed
  integer(int32) :: c

  integer(int32) :: kint32(3)

  kint32(1) = k
  kint32(2) = 11
  kint32(3) = seed_hash

  if (present(seed)) then
   c = hashint32(kint32, seed)
  else
   c = hashint32(kint32)
  end if
 end function

 pure function hashreal32_scal(k, seed) result(c)
  real(real32), intent(in) :: k
  integer(int32), intent(in), optional :: seed
  integer(int32) :: c

  integer(int32), allocatable :: kint32(:)

  allocate (kint32(1))
  kint32(1) = transfer(k, kint32(1))

  if (present(seed)) then
   c = hashint32(kint32, seed)
  else
   c = hashint32(kint32)
  end if
 end function

 elemental pure function roundinguppower2(x) result(next)
  integer(kind=int32), intent(in)::x
  integer(kind=int32)::next

  !https://stackoverflow.com/questions/466204/rounding-up-to-next-power-of-2

  next = 2_int32**int((ceiling(log(real(x, real64))/log(real(2, real64)))), int32)

 end function

!PRIVATE

 pure subroutine mix(a, b, c)
  integer(kind=int32), intent(inout) :: a, b, c

  a = a - c; a = ieor(a, rot(c, 4_int32)); c = c + b
  b = b - a; b = ieor(b, rot(a, 6_int32)); a = a + c
  c = c - b; c = ieor(c, rot(b, 8_int32)); b = b + a
  a = a - c; a = ieor(a, rot(c, 16_int32)); c = c + b
  b = b - a; b = ieor(b, rot(a, 19_int32)); a = a + c
  c = c - b; c = ieor(c, rot(b, 4_int32)); b = b + a

 end subroutine

 pure subroutine final(a, b, c)
  integer(kind=int32), intent(inout) :: a, b, c

  c = ieor(c, b); c = c - rot(b, 14_int32)
  a = ieor(a, c); a = a - rot(c, 11_int32)
  b = ieor(b, a); b = b - rot(a, 25_int32)
  c = ieor(c, b); c = c - rot(b, 16_int32)
  a = ieor(a, c); a = a - rot(c, 4_int32)
  b = ieor(b, a); b = b - rot(a, 14_int32)
  c = ieor(c, b); c = c - rot(b, 24_int32)

 end subroutine

 pure function rot(i, j) result(rota)
  integer(kind=int32), intent(in) :: i, j
  integer(kind=int32) :: rota

  rota = ior(ishft(i, j), ishft(i, -(32_int32 - j)))

 end function

end module
