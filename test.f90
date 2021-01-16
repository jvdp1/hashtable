module modhash
 use iso_fortran_env,only:int32,int64,real64
 implicit none
 private
 public::hashchar,hashint32,roundinguppower2
contains

pure function hashchar(k) result(c)
 character(len=*), intent(in) :: k
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

 c = hashint32(kint32)
end function

pure function hashint32(k) result(c)
 integer(kind=int32),intent(in)::k(:)

 integer(kind=int32) :: length
 integer(kind=int32) :: a,b,c
 integer(kind=int32) :: pos
 
 integer(kind=int32), parameter :: seed = 305419896_int32

 length=size(k) 

 a=seed + ishft(length,2)
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

program test
 use modhash
 use iso_fortran_env,only:int32,int64,real64
 implicit none
 integer(int32):: dim

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
end program
