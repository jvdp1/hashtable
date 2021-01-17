program test_list
 use modhash, only: hash
 use iso_fortran_env, only: int32, real32
 implicit none
 integer(int32) :: seed

 !print*,hash('aaa')
! print*,hash(34)
 !print*,hash([1,2])
 !print*,hash(3.6_real32)
 !print*,hash([1.3, 4.5, 6.4])

 call check(hash('aaa') == -889379598, 'char')
 call check(hash('aaa    ') == -889379598, 'char trailing')
 call check(hash(34) == -1461206935, 'int32')
 call check(hash([1, 2]) == -1956420251, 'int32_array')
 call check(hash(3.6_real32) == 596552866, 'real32')
 call check(hash([1.3, 4.5, 6.4]) == -1963187561, 'real32 array')

 !provided seed: seed = 1

 !print*,hash('aaa',1)
 !print*,hash(34,1)
 !print*,hash([1,2],1)
 !print*,hash(3.6_real32,1)
 !print*,hash([1.3, 4.5, 6.4],1)

 seed = 1
 call check(hash('aaa', seed) == -2045678163, 'char')
 call check(hash('aaa   ', seed) == -2045678163, 'char trailing')
 call check(hash(34, seed) == 912783350, 'int32')
 call check(hash([1, 2], seed) == -1736868386, 'int32_array')
 call check(hash(3.6_real32, seed) == -553270492, 'real32')
 call check(hash([1.3, 4.5, 6.4], seed) == 1124203056, 'real32 array')

contains

 subroutine check(lcheck, a)
  logical, intent(in) :: lcheck
  character(*), intent(in), optional :: a

  if (.not. lcheck) then
   if (present(a)) print'(2a)', 'ERROR: ', a
   error stop
  end if
 end subroutine

end program
