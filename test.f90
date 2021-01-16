module modhash
 use iso_fortran_env,only:int32,int64,real64
 implicit none

contains

pure function hashword(k) result(c)
 integer(kind=int32),intent(in)::k(:)

 integer(kind=int32) :: length
 integer(kind=int32) :: a,b,c
 integer(kind=int32) :: pos
 
 integer(kind=int32), parameter :: seed = 305419896_int32

 length=size(k) 

 a=seed + ishft(length,2) !4byte
 b=a
 c=a

 pos=0
 do while (length > 3)
  a=a+k(pos+1)
  b=b+k(pos+2)
  c=c+k(pos+3)
  call mix(a,b,c)
  length=length-3
  pos=pos+3
 end do 

 select case(length)
  case(1)
   a=a+k(pos+1)
  case(2)
   a=a+k(pos+1)
   b=b+k(pos+2)
  case(3)
   a=a+k(pos+1)
   b=b+k(pos+2)
   c=c+k(pos+3)
  case default
 end select

 call final(a,b,c)


end function

function roundinguppower2(x) result(next)
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

 print*,selected_int_kind(15),int64

end program
