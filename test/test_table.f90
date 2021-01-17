program test_table
 use iso_fortran_env, only: int32
 use modtable, only: tablechar_t, tableint32_t, tablereal32_t, table_arrint32_t
 implicit none
 integer(int32) :: pos
 type(tablechar_t) :: table
 type(tableint32_t) :: table32
 type(tablereal32_t) :: tabler32
 type(table_arrint32_t) :: tablearri32


 call test_char()

 table = tablechar_t(12)

 call table%add('a')
 call table%add('ab')
 call table%add('abc')
 call table%add('abcdd')
 call table%add('1')
 call table%add('   1')
 call table%add('   1    ')
 call table%add('12')
 call table%add('123')
 call table%add('1234')
 call table%add('12345')
 call table%add('12346')
 call table%add('12347')
 call table%add('12348')
 call table%writetable('table0.dat')
 call table%add('12349')
 call table%add('12350')
 call table%add('12351')
 call table%add('  12352')
 call table%add('12353')
 call table%add('12353')
 call table%add('12353')
 call table%add('12353   ')

 call table%writetable('table.dat')

 table32 = tableint32_t()

 call table32%add(1)
 call table32%add(2)
 call table32%add(5)
 call table32%writetable('tableint32.dat')

 tabler32 = tablereal32_t()

 call tabler32%add(1.)
 call tabler32%add(2.)
 call tabler32%add(5.)
 call tabler32%writetable('tablereal32.dat')


 tablearri32 = table_arrint32_t(3)

 call tablearri32%add([1,2,3])
 call tablearri32%add([3,2,3])
 call tablearri32%add([1,2,2])

 call tablearri32%add([1,2,2,3], pos)
 print*,'pos ', pos

 call tablearri32%writetable('tablearri32.dat')

contains
subroutine test_char()
 type(tablechar_t) :: table
 type(tablechar_t) :: table1

 integer :: i,j, io, un
 integer, parameter:: lenchar=12
 character(len=:), allocatable :: namefile
 character(len=lenchar):: cdummy
 logical :: lnew

 table = tablechar_t(lenchar, nel=5)

 call check(table%getsize() == 8, 'issue with initial size')
 call check(table%getfilled() == 0, 'issue with initial filled')

 !empty file
 namefile='table_char.dat'
 call table%writetable(namefile)
 open(newunit=un,file=namefile,status='old',action='read',iostat=io)
 call check(io == 0, 'issue with empty '//namefile)
  
 i=0
 do
  read(un,*,iostat=io)
  if(io.ne.0)exit
  i=i+1
 enddo
 call check(i == 0, 'issue with empty '//namefile//': file not empty')
 call check(io == -1, 'io: issue with empty '//namefile)

 call table%add('a')
 call check(table%getfilled()==1 .and. table%getsize()==8, 'char: issue 1')

 call table%add('ab')
 call check(table%getfilled()==2 .and. table%getsize()==8, 'char: issue 2')

 call table%add('abc')
 call check(table%getfilled()==3 .and. table%getsize()==8, 'char: issue 3')

 call table%add('abcdd')
 call check(table%getfilled()==4 .and. table%getsize()==8, 'char: issue 4')

 call table%add('1')
 call check(table%getfilled()==5 .and. table%getsize()==8, 'char: issue 5')

 call table%add('   1')
 call check(table%getfilled()==6 .and. table%getsize()==8, 'char: issue 6')

 call table%add('    1')
 call check(table%getfilled()==7 .and. table%getsize()==8, 'char: issue 7')

 !increase table
 call table%add('12')
 call check(table%getfilled()==8 .and. table%getsize()==16, 'char: issue 8')

 call table%add('12     ',lnew = lnew)  !trailing blank are ignored
 call check(.not.lnew,'char: issue lnew 0')

 call table%add('   12    ',lnew = lnew)
 call check(lnew,'char: issue lnew 1')

 call table%add('1', i)
 call check(i==5, 'char: issue with index 0')

 call table%add('1', i, lnew)
 call check(i==5 .and. .not.lnew, 'char: issue with index 1')

 call table%add('?', i)
 call check(i==10, 'char: issue with index 2')

 call table%add('??', i, lnew)
 call check(i==11 .and. lnew, 'char: issue with index 3')

 call check(table%getindex('   1') == 6,'char: issue with getindex 0')
 call check(table%getindex('1   ') == 5,'char: issue with getindex 1')
 call check(table%getindex('xxxx') == -1,'char: issue with getindex 2')

 call check(table%get(10) == '?','char: issue with get 0')
 call check(table%get(6) == '   1','char: issue with get 1')

 print*,'char: get outside filled: ',table%get(table%getfilled()+1) !how to test that

 call table%writetable(namefile//'1')

 !read table, add to a new, and check
 table1=tablechar_t(lenchar)
 namefile=namefile//'1'
 open(newunit=un,file=namefile,status='old',action='read',iostat=io)
 call check(io == 0, 'issue with '//namefile)
  
 i=0
 do
  read(un,'(i10,1x,a)',iostat=io)j,cdummy
  if(io.ne.0)exit
  call table1%add(cdummy)
  call check(table%getindex(cdummy) == table1%getindex(cdummy), 'char: issue 0 '//cdummy)
  call check(table%get(j) == table1%get(j), 'char: issue 1 '//cdummy)
  call check(j == table1%getindex(cdummy), 'char: issue 2 '//cdummy)
  call check(cdummy == table1%get(j), 'char: issue 3 '//cdummy)
  i=i+1
 enddo
 call check(io == -1, 'char: io: issue with '//namefile)
 call check(i == 11, 'char: issue with '//namefile//': file not correct')

 call check(table%getfilled() == table1%getfilled(), 'char: issue table1 0')


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
