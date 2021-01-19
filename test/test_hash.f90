program test_list
 use modhash, only: hash
 use iso_fortran_env, only: int32, real32
 implicit none
 integer(int32) :: seed

 !print*,hash('a ')
 !print*,hash('ab ')
 !print*,hash('abc ')
 !print*,hash('abcd ')
 !print*,hash('abcd1 ')
 !print*,hash('abcd12 ')
 !print*,hash('abcd123 ')
 !print*,hash('abcd1234 ')
 !print*,hash('abcd1234x ')
 !print*,hash('aaa')
 !print*,hash(' aaa')
 !print*,hash(34)
 !print*,hash([1,2])
 !print*,hash(3.6_real32)
 !print*,hash([1.3, 4.5, 6.4])

 call check(hash('a ') == 254356765, 'char 1')
 call check(hash('ab ') == -1993525330, 'char 2')
 call check(hash('abc ') == -459131787, 'char 3')
 call check(hash('abcd ') == -2085448470, 'char 4')
 call check(hash('abcd1 ') == -1674863665, 'char 5')
 call check(hash('abcd12 ') == 515185556, 'char 6')
 call check(hash('abcd123 ') == -296722923, 'char 7')
 call check(hash('abcd1234 ') == 1686877974, 'char 8')
 call check(hash('abcd1234x ') == 1770782053, 'char 9')

 call check(hash('aaa') == 1769952254, 'char')
 call check(hash('aaa    ') == 1769952254, 'char trailing')
 call check(hash(' aaa   ') == -1889864426, 'char preceding')
 call check(hash(34) == -1461206935, 'int32')
 call check(hash([1, 2]) == -1956420251, 'int32_array')
 call check(hash(3.6_real32) == 596552866, 'real32')
 call check(hash([1.3, 4.5, 6.4]) == -1963187561, 'real32 array')

 !provided seed: seed = 1
 seed = 1

 !print*,hash('a ',seed)
 !print*,hash('ab ',seed)
 !print*,hash('abc ',seed)
 !print*,hash('abcd ',seed)
 !print*,hash('abcd1 ',seed)
 !print*,hash('abcd12 ',seed)
 !print*,hash('abcd123 ',seed)
 !print*,hash('abcd1234 ',seed)
 !print*,hash('abcd1234x ',seed)
 !print*,hash('aaa',seed)
 !print*,hash(' aaa',seed)
 !print*,hash(34,seed)
 !print*,hash([1,2],seed)
 !print*,hash(3.6_real32,seed)
 !print*,hash([1.3, 4.5, 6.4],seed)


 call check(hash('a ',1) == -1653972736, 'char 1')
 call check(hash('ab ',1) == 1023604215, 'char 2')
 call check(hash('abc ',1) == -1449569323, 'char 3')
 call check(hash('abcd ',1) == 641420148, 'char 4')
 call check(hash('abcd1 ',1) == -1035568394, 'char 5')
 call check(hash('abcd12 ',1) == -811249315, 'char 6')
 call check(hash('abcd123 ',1) == 683399948, 'char 7')
 call check(hash('abcd1234 ',1) == 905776060, 'char 8')
 call check(hash('abcd1234x ',1) == -1516804990, 'char 9')

 call check(hash('aaa', seed) == -980364053, 'seed char')
 call check(hash('aaa   ', seed) == -980364053, 'seed char trailing')
 call check(hash(' aaa   ', seed) == 1601411871, 'seed char preceding')
 call check(hash(34, seed) == 912783350, 'seed int32')
 call check(hash([1, 2], seed) == -1736868386, 'seed int32_array')
 call check(hash(3.6_real32, seed) == -553270492, 'seed real32')
 call check(hash([1.3, 4.5, 6.4], seed) == 1124203056, 'seed real32 array')

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
