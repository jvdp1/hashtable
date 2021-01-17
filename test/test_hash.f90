program test_list
 use modhash
 use iso_fortran_env,only:int32,int64,real64
 implicit none
 integer(int32) :: dim

 dim=roundinguppower2(1000)

 print*,hash([1,2,3,4,5]),iand(hash([1,2,3,4,5]),dim-1)+1
 print*,hash([1,2,3,4,5,6,7,8,9]),iand(hash([1,2,3,4,5,6,7,8,9]),dim-1)+1
 print*,hash([1,2,3,4,5,6,7,8,9,10]),iand(hash([1,2,3,4,5,6,7,8,9,10]),dim-1)+1
 print*,hash([1,2,3]),iand(hash([1,2,3]),dim-1)+1
 print*,hash([1,2]),iand(hash([1,2]),dim-1)+1


 print*,'aaa ',hash('a')
 print*,'aaa ',hash('ab')
 print*,'aaa ',hash('abc')
 print*,'aaa ',hash('abc ')
 print*,'aaa ',hash(' abc')
 print*,'aaa ',hash(' abc  afafa')
 print*,'aaa ',hash(' abc  afafa 1234566')
 print*,'aaa ',hash(' abc  afafa 1234566 pppppppppp')

end program
