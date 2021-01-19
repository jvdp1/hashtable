program test_table
 use iso_fortran_env, only: int32, real32
 use modtable, only: tablechar_t, tableint32_t, tablereal32_t, table_arrint32_t, table_arrreal32_t
 implicit none

 call test_char()
 call test_int32()
 call test_real32()

 call test_arrint32()
 call test_arrreal32()

contains
 subroutine test_char()
  type(tablechar_t) :: table
  type(tablechar_t) :: table1

  integer :: i, j, io, un
  integer, parameter:: lenchar = 12
  character(len=:), allocatable :: namefile
  character(len=lenchar):: cdummy
  logical :: lnew

  table = tablechar_t(lenchar, nel=5)

  call check(table%getsize() == 8, 'issue with initial size')
  call check(table%getfilled() == 0, 'issue with initial filled')

  !empty file
  namefile = 'table_char.dat'
  call table%writetable(namefile)
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with empty '//namefile)

  i = 0
  do
   read (un, *, iostat=io)
   if (io .ne. 0) exit
   i = i + 1
  end do
  call check(i == 0, 'issue with empty '//namefile//': file not empty')
  call check(io == -1, 'io: issue with empty '//namefile)

  call table%add('a')
  call check(table%getfilled() == 1 .and. table%getsize() == 8, 'char: issue 1')

  call table%add('ab')
  call check(table%getfilled() == 2 .and. table%getsize() == 8, 'char: issue 2')

  call table%add('abc')
  call check(table%getfilled() == 3 .and. table%getsize() == 8, 'char: issue 3')

  call table%add('abcdd')
  call check(table%getfilled() == 4 .and. table%getsize() == 8, 'char: issue 4')

  call table%add('1')
  call check(table%getfilled() == 5 .and. table%getsize() == 8, 'char: issue 5')

  call table%add('   1')
  call check(table%getfilled() == 6 .and. table%getsize() == 8, 'char: issue 6')

  call table%add('    1')
  call check(table%getfilled() == 7 .and. table%getsize() == 8, 'char: issue 7')

  !increase table
  call table%add('12')
  call check(table%getfilled() == 8 .and. table%getsize() == 16, 'char: issue 8')

  call table%add('12     ', lnew=lnew)  !trailing blank are ignored
  call check(.not. lnew, 'char: issue lnew 0')

  call table%add('   12    ', lnew=lnew)
  call check(lnew, 'char: issue lnew 1')

  call table%add('1', i)
  call check(i == 5, 'char: issue with index 0')

  call table%add('1', i, lnew)
  call check(i == 5 .and. .not. lnew, 'char: issue with index 1')

  call table%add('?', i)
  call check(i == 10, 'char: issue with index 2')

  call table%add('??', i, lnew)
  call check(i == 11 .and. lnew, 'char: issue with index 3')

  call check(table%getindex('   1') == 6, 'char: issue with getindex 0')
  call check(table%getindex('1   ') == 5, 'char: issue with getindex 1')
  call check(table%getindex('xxxx') == -2, 'char: issue with getindex 2')

  call check(table%get(10) == '?', 'char: issue with get 0')
  call check(table%get(6) == '   1', 'char: issue with get 1')

  print *, 'char: get outside filled: ', table%get(table%getfilled() + 1) !how to test that

  call table%writetable(namefile//'1')

  !read table, add to a new, and check
  table1 = tablechar_t(lenchar)
  namefile = namefile//'1'
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with '//namefile)

  i = 0
  do
   read (un, '(i10,1x,a)', iostat=io) j, cdummy
   if (io .ne. 0) exit
   call table1%add(cdummy)
   call check(table%getindex(cdummy) == table1%getindex(cdummy), 'char: issue 0 '//cdummy)
   call check(table%get(j) == table1%get(j), 'char: issue 1 '//cdummy)
   call check(j == table1%getindex(cdummy), 'char: issue 2 '//cdummy)
   call check(cdummy == table1%get(j), 'char: issue 3 '//cdummy)
   i = i + 1
  end do
  call check(io == -1, 'char: io: issue with '//namefile)
  call check(i == 11, 'char: issue with '//namefile//': file not correct')

  call check(table%getfilled() == table1%getfilled(), 'char: issue table1 0')

  print *, 'Succesful char'

 end subroutine

 subroutine test_int32()
  type(tableint32_t) :: table
  type(tableint32_t) :: table1

  integer :: i, j, io, un
  integer(int32) :: k
  character(len=:), allocatable :: namefile
  logical :: lnew

  table = tableint32_t(nel=5)

  call check(table%getsize() == 8, 'int32: issue with initial size')
  call check(table%getfilled() == 0, 'int32: issue with initial filled')

  !empty file
  namefile = 'table_int32.dat'
  call table%writetable(namefile)
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with empty '//namefile)

  i = 0
  do
   read (un, *, iostat=io)
   if (io .ne. 0) exit
   i = i + 1
  end do
  call check(i == 0, 'issue with empty '//namefile//': file not empty')
  call check(io == -1, 'io: issue with empty '//namefile)

  call table%add(10)
  call check(table%getfilled() == 1 .and. table%getsize() == 8, 'int32: issue 1')

  call table%add(11)
  call check(table%getfilled() == 2 .and. table%getsize() == 8, 'int32: issue 2')

  call table%add(111)
  call check(table%getfilled() == 3 .and. table%getsize() == 8, 'int32: issue 3')

  call table%add(2222)
  call check(table%getfilled() == 4 .and. table%getsize() == 8, 'int32: issue 4')

  call table%add(22)
  call check(table%getfilled() == 5 .and. table%getsize() == 8, 'int32: issue 5')

  call table%add(333)
  call check(table%getfilled() == 6 .and. table%getsize() == 8, 'int32: issue 6')

  call table%add(444)
  call check(table%getfilled() == 7 .and. table%getsize() == 16, 'int32: issue 7')

  call table%add(5)
  call check(table%getfilled() == 8 .and. table%getsize() == 16, 'int32: issue 8')

  call table%add(5, lnew=lnew)
  call check(.not. lnew, 'int32: issue lnew 0')

  call table%add(66, lnew=lnew)
  call check(lnew, 'int32: issue lnew 1')

  call table%add(22, i)
  call check(i == 5, 'int32: issue with index 0')

  call table%add(22, i, lnew)
  call check(i == 5 .and. .not. lnew, 'int32: issue with index 1')

  call table%add(19, i)
  call check(i == 10, 'int32: issue with index 2')

  call table%add(199, i, lnew)
  call check(i == 11 .and. lnew, 'int32: issue with index 3')

  call check(table%getindex(333) == 6, 'int32: issue with getindex 0')
  call check(table%getindex(22) == 5, 'int32: issue with getindex 1')

  call check(table%getindex(huge(k)) == -2, 'int32: issue with getindex 2')

  call check(table%get(10) == 19, 'int32: issue with get 0')
  call check(table%get(6) == 333, 'int32: issue with get 1')

  print *, 'int32: get outside filled: ', table%get(table%getfilled() + 1) !how to test that

  call table%writetable(namefile//'1')

  !read table, add to a new, and check
  table1 = tableint32_t()
  namefile = namefile//'1'
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with '//namefile)

  i = 0
  do
   read (un, *, iostat=io) j, k
   if (io .ne. 0) exit
   call table1%add(k)
   call check(table%getindex(k) == table1%getindex(k), 'int32: issue 0 ')
   call check(table%get(j) == table1%get(j), 'int32: issue 1 ')
   call check(j == table1%getindex(k), 'int32: issue 2 ')
   call check(k == table1%get(j), 'int32: issue 3 ')
   i = i + 1
  end do
  call check(io == -1, 'int32: io: issue with '//namefile)
  call check(i == 11, 'int32: issue with '//namefile//': file not correct')

  call check(table%getfilled() == table1%getfilled(), 'int32: issue table1 0')

  print *, 'Succesful int32'

 end subroutine

 subroutine test_real32()
  type(tablereal32_t) :: table
  type(tablereal32_t) :: table1

  integer :: i, j, io, un
  real(real32) :: k
  character(len=:), allocatable :: namefile
  logical :: lnew

  table = tablereal32_t(nel=5)

  call check(table%getsize() == 8, 'real32: issue with initial size')
  call check(table%getfilled() == 0, 'real32: issue with initial filled')

  !empty file
  namefile = 'table_real32.dat'
  call table%writetable(namefile)
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with empty '//namefile)

  i = 0
  do
   read (un, *, iostat=io)
   if (io .ne. 0) exit
   i = i + 1
  end do
  call check(i == 0, 'issue with empty '//namefile//': file not empty')
  call check(io == -1, 'io: issue with empty '//namefile)

  call table%add(10.)
  call check(table%getfilled() == 1 .and. table%getsize() == 8, 'real32: issue 1')

  call table%add(11.)
  call check(table%getfilled() == 2 .and. table%getsize() == 8, 'real32: issue 2')

  call table%add(111.)
  call check(table%getfilled() == 3 .and. table%getsize() == 8, 'real32: issue 3')

  call table%add(2222.)
  call check(table%getfilled() == 4 .and. table%getsize() == 8, 'real32: issue 4')

  call table%add(22.)
  call check(table%getfilled() == 5 .and. table%getsize() == 8, 'real32: issue 5')

  call table%add(333.)
  call check(table%getfilled() == 6 .and. table%getsize() == 8, 'real32: issue 6')

  call table%add(444.)
  call check(table%getfilled() == 7 .and. table%getsize() == 16, 'real32: issue 7')

  call table%add(5.)
  call check(table%getfilled() == 8 .and. table%getsize() == 16, 'real32: issue 8')

  call table%add(5., lnew=lnew)
  call check(.not. lnew, 'real32: issue lnew 0')

  call table%add(66., lnew=lnew)
  call check(lnew, 'real32: issue lnew 1')

  call table%add(22., i)
  call check(i == 5, 'real32: issue with index 0')

  call table%add(22., i, lnew)
  call check(i == 5 .and. .not. lnew, 'real32: issue with index 1')

  call table%add(19., i)
  call check(i == 10, 'real32: issue with index 2')

  call table%add(199., i, lnew)
  call check(i == 11 .and. lnew, 'real32: issue with index 3')

  call check(table%getindex(333.) == 6, 'real32: issue with getindex 0')
  call check(table%getindex(22.) == 5, 'real32: issue with getindex 1')

  call check(table%getindex(huge(k)) == -2, 'real32: issue with getindex 2')

  call check(table%get(10) == 19, 'real32: issue with get 0')
  call check(table%get(6) == 333, 'real32: issue with get 1')

  print *, 'real32: get outside filled: ', table%get(table%getfilled() + 1) !how to test that

  call table%writetable(namefile//'1')

  !read table, add to a new, and check
  table1 = tablereal32_t()
  namefile = namefile//'1'
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with '//namefile)

  i = 0
  do
   read (un, *, iostat=io) j, k
   if (io .ne. 0) exit
   call table1%add(k)
   call check(table%getindex(k) == table1%getindex(k), 'real32: issue 0 ')
   call check(table%get(j) == table1%get(j), 'real32: issue 1 ')
   call check(j == table1%getindex(k), 'real32: issue 2 ')
   call check(k == table1%get(j), 'real32: issue 3 ')
   i = i + 1
  end do
  call check(io == -1, 'real32: io: issue with '//namefile)
  call check(i == 11, 'real32: issue with '//namefile//': file not correct')

  call check(table%getfilled() == table1%getfilled(), 'real32: issue table1 0')

  print *, 'Succesful real32'

 end subroutine

 subroutine test_arrint32()
  type(table_arrint32_t) :: table
  type(table_arrint32_t) :: table1

  integer :: i, j, io, un
  integer(int32) :: k(3)
  character(len=:), allocatable :: namefile
  logical :: lnew

  table = table_arrint32_t(3, nel=5)

  call check(table%getsize() == 8, '_arrint32: issue with initial size')
  call check(table%getfilled() == 0, '_arrint32: issue with initial filled')

  !empty file
  namefile = 'table__arrint32.dat'
  call table%writetable(namefile)
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with empty '//namefile)

  i = 0
  do
   read (un, *, iostat=io)
   if (io .ne. 0) exit
   i = i + 1
  end do
  call check(i == 0, 'issue with empty '//namefile//': file not empty')
  call check(io == -1, 'io: issue with empty '//namefile)

  call table%add([10, 1, 1])
  call check(table%getfilled() == 1 .and. table%getsize() == 8, '_arrint32: issue 1')

  call table%add([11, 2, 1])
  call check(table%getfilled() == 2 .and. table%getsize() == 8, '_arrint32: issue 2')

  call table%add([111, 3, 1])
  call check(table%getfilled() == 3 .and. table%getsize() == 8, '_arrint32: issue 3')

  call table%add([2222, 4, 1])
  call check(table%getfilled() == 4 .and. table%getsize() == 8, '_arrint32: issue 4')

  call table%add([22, 3, 1])
  call check(table%getfilled() == 5 .and. table%getsize() == 8, '_arrint32: issue 5')

  call table%add([333, 1, 1])
  call check(table%getfilled() == 6 .and. table%getsize() == 16, '_arrint32: issue 6')

  call table%add([444, 1, 1])
  call check(table%getfilled() == 7 .and. table%getsize() == 16, '_arrint32: issue 7')

  call table%add([5, 1, 1])
  call check(table%getfilled() == 8 .and. table%getsize() == 16, '_arrint32: issue 8')

  call table%add([5, 1, 1], lnew=lnew)
  call check(.not. lnew, '_arrint32: issue lnew 0')

  call table%add([66, 1, 1], lnew=lnew)
  call check(lnew, '_arrint32: issue lnew 1')

  call table%add([22, 3, 1], i)
  call check(i == 5, '_arrint32: issue with index 0')

  call table%add([22, 3, 1], i, lnew)
  call check(i == 5 .and. .not. lnew, '_arrint32: issue with index 1')

  call table%add([19, 1, 1], i)
  call check(i == 10, '_arrint32: issue with index 2')

  call table%add([199, 1, 1], i, lnew)
  call check(i == 11 .and. lnew, '_arrint32: issue with index 3')

  call check(table%getindex([333, 1, 1]) == 6, '_arrint32: issue with getindex 0')
  call check(table%getindex([22, 3, 1]) == 5, '_arrint32: issue with getindex 1')

  call check(table%getindex([huge(k(1)), 1, 1]) == -1, '_arrint32: issue with getindex 2')

  call check(all(table%get(10) == [19, 1, 1]), '_arrint32: issue with get 0')
  call check(all(table%get(6) == [333, 1, 1]), '_arrint32: issue with get 1')

  print *, '_arrint32: get outside filled: ', table%get(table%getfilled() + 1) !how to test that

  call table%writetable(namefile//'1')

  !read table, add to a new, and check
  table1 = table_arrint32_t(3)
  namefile = namefile//'1'
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with '//namefile)

  i = 0
  do
   read (un, *, iostat=io) j, k
   if (io .ne. 0) exit
   call table1%add(k)
   call check(table%getindex(k) == table1%getindex(k), '_arrint32: issue 0 ')
   call check(all(table%get(j) == table1%get(j)), '_arrint32: issue 1 ')
   call check(j == table1%getindex(k), '_arrint32: issue 2 ')
   call check(all(k == table1%get(j)), '_arrint32: issue 3 ')
   i = i + 1
  end do
  call check(io == -1, '_arrint32: io: issue with '//namefile)
  call check(i == 11, '_arrint32: issue with '//namefile//': file not correct')

  call check(table%getfilled() == table1%getfilled(), '_arrint32: issue table1 0')

  print *, 'Succesful _arrint32'

 end subroutine

 subroutine test_arrreal32()
  type(table_arrreal32_t) :: table
  type(table_arrreal32_t) :: table1

  integer :: i, j, io, un
  real(real32) :: k(3)
  character(len=:), allocatable :: namefile
  logical :: lnew

  table = table_arrreal32_t(3, nel=5)

  call check(table%getsize() == 8, '_arrreal32: issue with initial size')
  call check(table%getfilled() == 0, '_arrreal32: issue with initial filled')

  !empty file
  namefile = 'table__arrreal32.dat'
  call table%writetable(namefile)
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with empty '//namefile)

  i = 0
  do
   read (un, *, iostat=io)
   if (io .ne. 0) exit
   i = i + 1
  end do
  call check(i == 0, 'issue with empty '//namefile//': file not empty')
  call check(io == -1, 'io: issue with empty '//namefile)

  call table%add([10., 1., 1.])
  call check(table%getfilled() == 1 .and. table%getsize() == 8, '_arrreal32: issue 1')

  call table%add([11., 2., 1.])
  call check(table%getfilled() == 2 .and. table%getsize() == 8, '_arrreal32: issue 2')

  call table%add([111., 3., 1.])
  call check(table%getfilled() == 3 .and. table%getsize() == 8, '_arrreal32: issue 3')

  call table%add([2222., 4., 1.])
  call check(table%getfilled() == 4 .and. table%getsize() == 8, '_arrreal32: issue 4')

  call table%add([22., 3., 1.])
  call check(table%getfilled() == 5 .and. table%getsize() == 8, '_arrreal32: issue 5')

  call table%add([333., 1., 1.])
  call check(table%getfilled() == 6 .and. table%getsize() == 8, '_arrreal32: issue 6')

  call table%add([444., 1., 1.])
  call check(table%getfilled() == 7 .and. table%getsize() == 16, '_arrreal32: issue 7')

  call table%add([5., 1., 1.])
  call check(table%getfilled() == 8 .and. table%getsize() == 16, '_arrreal32: issue 8')

  call table%add([5., 1., 1.], lnew=lnew)
  call check(.not. lnew, '_arrreal32: issue lnew 0')

  call table%add([66., 1., 1.], lnew=lnew)
  call check(lnew, '_arrreal32: issue lnew 1')

  call table%add([22., 3., 1.], i)
  call check(i == 5, '_arrreal32: issue with index 0')

  call table%add([22., 3., 1.], i, lnew)
  call check(i == 5 .and. .not. lnew, '_arrreal32: issue with index 1')

  call table%add([19., 1., 1.], i)
  call check(i == 10, '_arrreal32: issue with index 2')

  call table%add([199., 1., 1.], i, lnew)
  call check(i == 11 .and. lnew, '_arrreal32: issue with index 3')

  call check(table%getindex([333., 1., 1.]) == 6, '_arrreal32: issue with getindex 0')
  call check(table%getindex([22., 3., 1.]) == 5, '_arrreal32: issue with getindex 1')

  call check(table%getindex([huge(k(1)), 1., 1.]) == -1, '_arrreal32: issue with getindex 2')

  call check(all(table%get(10) == [19., 1., 1.]), '_arrreal32: issue with get 0')
  call check(all(table%get(6) == [333., 1., 1.]), '_arrreal32: issue with get 1')

  print *, '_arrreal32: get outside filled: ', table%get(table%getfilled() + 1) !how to test that

  call table%writetable(namefile//'1')

  !read table, add to a new, and check
  table1 = table_arrreal32_t(3)
  namefile = namefile//'1'
  open (newunit=un, file=namefile, status='old', action='read', iostat=io)
  call check(io == 0, 'issue with '//namefile)

  i = 0
  do
   read (un, *, iostat=io) j, k
   if (io .ne. 0) exit
   call table1%add(k)
   call check(table%getindex(k) == table1%getindex(k), '_arrreal32: issue 0 ')
   call check(all(table%get(j) == table1%get(j)), '_arrreal32: issue 1 ')
   call check(j == table1%getindex(k), '_arrreal32: issue 2 ')
   call check(all(k == table1%get(j)), '_arrreal32: issue 3 ')
   i = i + 1
  end do
  call check(io == -1, '_arrreal32: io: issue with '//namefile)
  call check(i == 11, '_arrreal32: issue with '//namefile//': file not correct')

  call check(table%getfilled() == table1%getfilled(), '_arrreal32: issue table1 0')

  print *, 'Succesful _arrreal32'

 end subroutine

 subroutine check(lcheck, a)
  logical, intent(in) :: lcheck
  character(*), intent(in), optional :: a

  if (.not. lcheck) then
   if (present(a)) print'(2a)', 'ERROR: ', a
   error stop
  end if
 end subroutine

end program
