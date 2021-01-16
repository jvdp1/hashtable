module modhash
 use iso_fortran_env,only:int32,real64
 implicit none
 private
 public::hashchar,hashint32,roundinguppower2

 integer(int32), parameter, public :: seed_hash = 305419896_int32

contains

pure function hashchar(k, seed) result(c)
 character(len=*), intent(in) :: k
 integer(int32), intent(in), optional :: seed
 integer(int32) :: c
 
 integer(int32) :: lenk, length, i
 integer(int32), allocatable :: kint32(:)

 lenk = len(k)

 length = int(lenk/4_int32)
 if(mod(lenk,4).ne.0) length = length + 1

 allocate(kint32(length))

 do i=1,lenk,4
  kint32(i) = transfer(k(i:min(i+3,lenk)),kint32(i))
 enddo
 
 if(present(seed))then
  c = hashint32(kint32, seed)
 else
  c = hashint32(kint32)
 endif
end function

pure function hashint32(k, seed) result(c)
 integer(kind=int32),intent(in)::k(:)
 integer(kind=int32), intent(in), optional :: seed

 integer(kind=int32) :: length
 integer(kind=int32) :: a,b,c
 integer(kind=int32) :: pos
 
 integer(kind=int32) :: seed_

 seed_ = seed_hash
 if(present(seed)) seed_ = seed

 length=size(k) 

 a=seed_ + ishft(length,2)
 b=a
 c=a

 pos=1
 do while (length > 3)
  a=a+k(pos)
  b=b+k(pos+1)
  c=c+k(pos+2)
  call mix(a,b,c)
  length=length-3
  pos=pos+3
 end do 

 select case(length)
  case(1)
   a=a+k(pos)
  case(2)
   a=a+k(pos)
   b=b+k(pos+1)
  case(3)
   a=a+k(pos)
   b=b+k(pos+1)
   c=c+k(pos+2)
  case default
 end select

 call final(a,b,c)


end function

elemental pure function roundinguppower2(x) result(next)
 integer(kind=int32),intent(in)::x
 integer(kind=int32)::next

 !https://stackoverflow.com/questions/466204/rounding-up-to-next-power-of-2
 
 next=2_int32**int((ceiling(log(real(x,real64))/log(real(2,real64)))),int32)

end function

!PRIVATE
    
pure subroutine mix(a,b,c)
 integer(kind=int32),intent(inout)::a,b,c

 a=a-c;a=ieor(a,rot(c,4_int32));c=c+b 
 b=b-a;b=ieor(b,rot(a,6_int32));a=a+c 
 c=c-b;c=ieor(c,rot(b,8_int32));b=b+a 
 a=a-c;a=ieor(a,rot(c,16_int32));c=c+b 
 b=b-a;b=ieor(b,rot(a,19_int32));a=a+c 
 c=c-b;c=ieor(c,rot(b,4_int32));b=b+a 

end subroutine 

pure subroutine final(a,b,c)
 integer(kind=int32),intent(inout)::a,b,c

 c=ieor(c,b); c=c-rot(b,14_int32)
 a=ieor(a,c); a=a-rot(c,11_int32)
 b=ieor(b,a); b=b-rot(a,25_int32)
 c=ieor(c,b); c=c-rot(b,16_int32)
 a=ieor(a,c); a=a-rot(c,4_int32)
 b=ieor(b,a); b=b-rot(a,14_int32)
 c=ieor(c,b); c=c-rot(b,24_int32)

end subroutine

pure function rot(i,j) result(rota)
 integer(kind=int32),intent(in):: i,j
 integer(kind=int32)::rota

 rota=ior(ishft(i,j),ishft(i,-(32_int32-j)))

end function

end module

module modlist
 use iso_fortran_env,only:int32
 use modhash, only: roundinguppower2, seed_hash, hashchar
 implicit none
 private
 public::listchar_t

 type::listchar_t !(k)
  private
  !integer, len :: k
  integer(int32) :: filled
  integer(int32) :: nel
  integer(int32),allocatable::id(:)
  !character(len=k),allocatable::stored(:)
  character(len=:),allocatable::stored(:)
  contains
  private
  procedure, public :: add => add_listchar_t
  procedure :: destroy => destroy_listchar_t
  procedure, public :: get => get_listchar_t
  procedure, public :: getindex => getindex_listchar_t
  procedure, public :: writetable => writetable_listchar_t
  final :: destroy_listchar_t_scal, destroy_listchar_t_array1
 end type
 
 interface listchar_t
  module procedure constructor_listchar_t
 end interface

contains

pure function constructor_listchar_t(k,nel) result(this)
 type(listchar_t) :: this
 integer(kind=int32),intent(in) :: k
 integer(kind=int32),intent(in), optional :: nel

 this%nel = roundinguppower2(10)
 if(present(nel)) this%nel = roundinguppower2(nel)

 this%filled=0

 allocate(this%id(this%nel))
 this%id=0

 allocate(character(len=k) :: this%stored(this%nel))
 this%stored = ''

end function

recursive subroutine add_listchar_t(this, c, index, lnew)
 class(listchar_t), intent(inout) :: this
 character(len=*),intent(in) :: c
 integer(int32),intent(out),optional :: index
 logical, intent(out), optional :: lnew

 integer(int32) :: i, address
 integer(int32),parameter :: maxiter = 5000

 if(real(this%filled).gt.0.80*this%nel)call increase_size()

 address = seed_hash

 do i = 1, min(this%nel,maxiter)-1
  address = iand( hashchar(c, address), this%nel - 1) + 1

  if(this%id(address).eq.0)then
   this%filled = this%filled + 1
   this%id(address) = this%filled
   this%stored(this%id(address)) = c
   if(present(index)) index = this%id(address)
   if(present(lnew)) lnew = .true.
   return
  else !already occupied
   if(this%stored(this%id(address)).eq.c)then
    if(present(index)) index = this%id(address)
    if(present(lnew)) lnew = .false.
    return
   endif
  endif
 enddo

 call increase_size()

 address = seed_hash

 do i = 1, min(this%nel,maxiter)-1
  address = iand( hashchar(c, address), this%nel - 1) + 1

  if(this%id(address).eq.0)then
   this%filled = this%filled + 1
   this%id(address) = this%filled
   this%stored(this%id(address)) = c
   if(present(index)) index = this%id(address)
   if(present(lnew)) lnew = .true.
   return
  else !already occupied
   if(this%stored(this%id(address)).eq.c)then
    if(present(index)) index = this%id(address)
    if(present(lnew)) lnew = .false.
    return
   endif
  endif
 enddo


 address = -1
 if(present(index)) index = -1

 error stop 'too many collisions'

 contains

  subroutine increase_size()
   type(listchar_t) :: thistmp
  
   thistmp = listchar_t(len(this%stored), nel = int(this%nel*1.5, int32))
  
   do i = 1, this%filled
    call thistmp%add(this%stored(i))
   enddo
  
   this%filled = thistmp%filled
   this%nel = thistmp%nel
   if(allocated(this%id)) deallocate(this%id)
   if(allocated(this%stored)) deallocate(this%stored)
   call move_alloc(thistmp%id,this%id)
   call move_alloc(thistmp%stored,this%stored)
  
  end subroutine

end subroutine

pure subroutine destroy_listchar_t(this)
 class(listchar_t), intent(inout) :: this

 this%filled=0
 this%nel=0
 if(allocated(this%id))deallocate(this%id)
 if(allocated(this%stored))deallocate(this%stored)

end subroutine

pure subroutine destroy_listchar_t_scal(this)
 type(listchar_t), intent(inout) :: this

 call this%destroy()

end subroutine

pure subroutine destroy_listchar_t_array1(this)
 type(listchar_t), intent(inout) :: this(:)

 integer :: i

 do i = 1, size(this)
  call this(i)%destroy()
 enddo

end subroutine

pure function get_listchar_t(this, index) result(c)
 class(listchar_t), intent(in) :: this
 integer(int32), intent(in) :: index
 character(len=len(this%stored)) :: c

 c = this%stored(index)

end function

pure function getindex_listchar_t(this, c) result(index)
 class(listchar_t), intent(in) :: this
 character(*),intent(in) :: c
 integer(int32) :: index


 integer(int32) :: i, address
 integer(int32),parameter :: maxiter = 5000
 type(listchar_t),allocatable :: thistmp

 address = seed_hash

 do i = 1, min(this%nel,maxiter)-1
  address = iand( hashchar(c, address), this%nel - 1) + 1

  if(this%id(address).eq.0)then
   index=0
   return
  else !occupied
   if(this%stored(this%id(address)).eq.c)then
    index = this%id(address)
    return
   endif
  endif
 enddo

 index = -1

end function

subroutine writetable_listchar_t(this, namefile)
 class(listchar_t), intent(in) :: this
 character(*), intent(in) :: namefile

 integer :: i, un
 
 open(newunit=un, file=namefile, action = 'write')
 do i = 1, this%filled
  write(un,'(i0,x,a)')i,this%get(i)
 enddo
 close(un)

end subroutine

end module

program test
 use modhash
 use modlist, only: listchar_t
 use iso_fortran_env,only:int32,int64,real64
 implicit none
 integer(int32) :: dim
 type(listchar_t) :: list

 dim=roundinguppower2(1000)

 print*,hashint32([1,2,3,4,5]),iand(hashint32([1,2,3,4,5]),dim-1)+1
 print*,hashint32([1,2,3,4,5,6,7,8,9]),iand(hashint32([1,2,3,4,5,6,7,8,9]),dim-1)+1
 print*,hashint32([1,2,3,4,5,6,7,8,9,10]),iand(hashint32([1,2,3,4,5,6,7,8,9,10]),dim-1)+1
 print*,hashint32([1,2,3]),iand(hashint32([1,2,3]),dim-1)+1
 print*,hashint32([1,2]),iand(hashint32([1,2]),dim-1)+1


 print*,'aaa ',hashchar('a')
 print*,'aaa ',hashchar('ab')
 print*,'aaa ',hashchar('abc')
 print*,'aaa ',hashchar('abc ')
 print*,'aaa ',hashchar(' abc')
 print*,'aaa ',hashchar(' abc  afafa')
 print*,'aaa ',hashchar(' abc  afafa 1234566')
 print*,'aaa ',hashchar(' abc  afafa 1234566 pppppppppp')

 list = listchar_t(12)

 call list%add('a')
 call list%add('ab')
 call list%add('abc')
 call list%add('abcdd')
 call list%add('1')
 call list%add('12')
 call list%add('123')
 call list%add('1234')
 call list%add('12345')
 call list%add('12346')
 call list%add('12347')
 call list%add('12348')
 call list%writetable('list0.dat')
 call list%add('12349')
 call list%add('12350')
 call list%add('12351')
 call list%add('12352')
 call list%add('12353')
 call list%add('12353')
 call list%add('12353')

 call list%writetable('list.dat')
end program
