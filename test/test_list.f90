program test_list
 use iso_fortran_env, only: int32
 use modtable, only: table_char_t, table_int32_t
 implicit none

 call test_char()
 call test_int32()

contains

 subroutine test_char()
  integer(int32), parameter :: nchar = 10, nrows = 18000000
  integer(int32) :: i, j, un, n, io
  integer(int32) :: col
  real :: start, finish
  character(nchar) :: cdummy
  character(nchar), allocatable :: list(:)
  type(table_char_t), allocatable :: table

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
   allocate (table)
   table = table_char_t(nchar, nrows/j)

   print *, ' size  : ', table%getsize()

   call cpu_time(start)
   do i = 1, nrows
    call table%add(list(i))!,col=col)
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', table%getfilled()
   print *, ' size  : ', table%getsize()
!   print *, ' col   : ', col

   call cpu_time(start)
   do i = nrows, 1, -1
    call table%add(list(i))
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', table%getfilled()
   print *, ' size  : ', table%getsize()

   deallocate (table)

  end do

 end subroutine

 subroutine test_int32()
  integer(int32), parameter :: nrows = 18000000
  integer(int32) :: i, j, un, n, io
  integer(int32) :: col
  real :: start, finish
  integer(int32), allocatable :: list(:)
  type(table_int32_t), allocatable :: table

  allocate (list(nrows))

  open (newunit=un, file='list.dat', status='old', action='read')
  do i = 1, nrows
   read (un, *, iostat=io) list(i)
   if (io .ne. 0) exit
  end do
  close (un)

  do j = 5, 1, -1
   write (*, '(a,i0)') 'Start char table', j
   allocate (table)
   table = table_int32_t(nrows/j)

   print *, ' size  : ', table%getsize()

   call cpu_time(start)
   do i = 1, nrows
    call table%add(list(i))!,col=col)
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', table%getfilled()
   print *, ' size  : ', table%getsize()
!   print *, ' col   : ', col

   call cpu_time(start)
   do i = nrows, 1, -1
    call table%add(list(i))
   end do
   call cpu_time(finish)

   print '(" Time = ",f6.3," seconds.")', finish - start
   print *, ' filled: ', table%getfilled()
   print *, ' size  : ', table%getsize()

   deallocate (table)

  end do

 end subroutine

end program
