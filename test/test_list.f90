program test_list
 use iso_fortran_env, only: int32
 use modtable, only: tablechar_t, tableint32_t
 implicit none

 call test_char()

contains

 subroutine test_char()
  integer(int32), parameter :: nchar = 10, nrows = 18000!000
  integer(int32) :: i, j, un, n, io
  real :: start, finish
  character(nchar) :: cdummy
  character(nchar), allocatable :: list(:)
  type(tablechar_t), allocatable :: tablechar

  allocate (list(nrows))

  open (newunit=un, file='list.dat', status='old', action='read')
  do i = 1, nrows
   read (un, *, iostat=io) cdummy
   if (io .ne. 0) exit
   list(i) = cdummy
  end do
  close (un)

  do j = 5, 1, -1
   write (*, '(a,i0)') 'Start char table', j
   allocate (tablechar)
   tablechar = tablechar_t(nchar, nrows/j)

   print *, ' size  : ', tablechar%getsize()

   call cpu_time(start)
   do i = 1, nrows
    call tablechar%add(list(i))
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', tablechar%getfilled()
   print *, ' size  : ', tablechar%getsize()

   call cpu_time(start)
   do i = nrows, 1, -1
    call tablechar%add(list(i))
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', tablechar%getfilled()
   print *, ' size  : ', tablechar%getsize()

   deallocate (tablechar)

  end do

 end subroutine

end program
