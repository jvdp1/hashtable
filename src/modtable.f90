module modtable
 use iso_fortran_env, only: int32, real32
 use modhash, only: roundinguppower2, seed_hash, hash
 implicit none
 private
 public::table_char_t
 public::table_int32_t
 public::table_real32_t
 public::table_arrint32_t
 public::table_arrreal32_t

 integer(int32), parameter :: default_nel = 5000

 type, abstract :: tableabstract_t
  private
  integer(int32) :: filled
  integer(int32) :: nel
  integer(int32), allocatable :: id(:)
 contains
  private
  procedure(int_destroy_tableabstract_t), public, deferred :: destroy
  procedure :: destroy_tableabstract_t
  procedure, public :: getfilled => getfilled_tableabstract_t
  procedure, public :: getsize => getsize_tableabstract_t
 end type

 abstract interface
  pure subroutine int_destroy_tableabstract_t(this)
   import::tableabstract_t
   class(tableabstract_t), intent(inout) :: this
  end subroutine
 end interface

 type, extends(tableabstract_t) :: table_char_t !(k)
  private
  !integer, len :: k
  !character(len=k),allocatable::stored(:)
  character(len=:), allocatable ::stored(:)
 contains
  private
  procedure, public :: add => add_tablechar_t
  procedure, public :: destroy => destroy_tablechar_t
  procedure, public :: get => get_tablechar_t
  procedure, public :: getindex => getindex_tablechar_t
  procedure, public :: writetable => writetable_tablechar_t
  final :: destroy_tablechar_t_scal, destroy_tablechar_t_array1
 end type

 interface table_char_t
  module procedure constructor_tablechar_t
 end interface

 type, extends(tableabstract_t) :: table_int32_t !(k)
  private
  !integer, len :: k
  !character(len=k),allocatable::stored(:)
  integer(int32), allocatable ::stored(:)
 contains
  private
  procedure, public :: add => add_tableint32_t
  procedure, public :: destroy => destroy_tableint32_t
  procedure, public :: get => get_tableint32_t
  procedure, public :: getindex => getindex_tableint32_t
  procedure, public :: writetable => writetable_tableint32_t
  final :: destroy_tableint32_t_scal, destroy_tableint32_t_array1
 end type

 interface table_int32_t
  module procedure constructor_tableint32_t
 end interface

 type, extends(tableabstract_t) :: table_real32_t !(k)
  private
  !integer, len :: k
  !character(len=k),allocatable::stored(:)
  real(real32), allocatable ::stored(:)
 contains
  private
  procedure, public :: add => add_tablereal32_t
  procedure, public :: destroy => destroy_tablereal32_t
  procedure, public :: get => get_tablereal32_t
  procedure, public :: getindex => getindex_tablereal32_t
  procedure, public :: writetable => writetable_tablereal32_t
  final :: destroy_tablereal32_t_scal, destroy_tablereal32_t_array1
 end type

 interface table_real32_t
  module procedure constructor_tablereal32_t
 end interface


 type, extends(tableabstract_t) :: table_arrint32_t
  private
  integer(int32), allocatable ::stored(:, :)
 contains
  private
  procedure, public :: add => add_table_arrint32_t
  procedure, public :: destroy => destroy_table_arrint32_t
  procedure, public :: get => get_table_arrint32_t
  procedure, public :: getindex => getindex_table_arrint32_t
  procedure, public :: writetable => writetable_table_arrint32_t
  final :: destroy_table_arrint32_t_scal, destroy_table_arrint32_t_array1
 end type

 interface table_arrint32_t
  module procedure constructor_table_arrint32_t
 end interface

 type, extends(tableabstract_t) :: table_arrreal32_t
  private
  real(real32), allocatable ::stored(:, :)
 contains
  private
  procedure, public :: add => add_table_arrreal32_t
  procedure, public :: destroy => destroy_table_arrreal32_t
  procedure, public :: get => get_table_arrreal32_t
  procedure, public :: getindex => getindex_table_arrreal32_t
  procedure, public :: writetable => writetable_table_arrreal32_t
  final :: destroy_table_arrreal32_t_scal, destroy_table_arrreal32_t_array1
 end type

 interface table_arrreal32_t
  module procedure constructor_table_arrreal32_t
 end interface


contains

 pure subroutine destroy_tableabstract_t(this)
  class(tableabstract_t), intent(inout) :: this

  this%filled = 0
  this%nel = 0
  if (allocated(this%id)) deallocate (this%id)

 end subroutine

 pure function getfilled_tableabstract_t(this) result(filled)
  class(tableabstract_t), intent(in) :: this
  integer(int32) :: filled

  filled = this%filled

 end function

 pure function getsize_tableabstract_t(this) result(nel)
  class(tableabstract_t), intent(in) :: this
  integer(int32) :: nel

  nel = this%nel

 end function

 pure function constructor_tablechar_t(k, nel) result(this)
  integer(kind=int32), intent(in) :: k
   integer(kind=int32), intent(in), optional :: nel
   type(table_char_t) :: this

   this%nel = roundinguppower2(default_nel)
   if (present(nel)) this%nel = roundinguppower2(nel)

   this%filled = 0

   allocate (this%id(this%nel))
   this%id = 0

   allocate (character(len=k) :: this%stored(this%nel))
   this%stored(:) = ''

  end function

  recursive subroutine add_tablechar_t(this, c, index, lnew) !, col)
   class(table_char_t), intent(inout) :: this
   character(len=*), intent(in) :: c
   integer(int32), intent(out), optional :: index
   !integer(int32), intent(out), optional :: col
   logical, intent(out), optional :: lnew

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (real(this%filled) .gt. 0.80*this%nel) call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = i
      return
     end if
    end if
   end do

   call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = min(this%nel, maxiter) - 1 + i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = min(this%nel, maxiter) - 1 + i
      return
     end if
    end if
   end do

   address = -1
   if (present(index)) index = -1

   error stop 'too many collisions'

    contains

     subroutine increase_size()
      type(table_char_t) :: thistmp

       thistmp = table_char_t(len(this%stored), nel=int(this%nel*1.5, int32))

      do i = 1, this%filled
       call thistmp%add(this%stored(i))
      end do

      this%filled = thistmp%filled
      this%nel = thistmp%nel
      if (allocated(this%id)) deallocate (this%id)
      if (allocated(this%stored)) deallocate (this%stored)
      call move_alloc(thistmp%id, this%id)
      call move_alloc(thistmp%stored, this%stored)

     end subroutine

  end subroutine

  pure subroutine destroy_tablechar_t(this)
   class(table_char_t), intent(inout) :: this

   call this%destroy_tableabstract_t

   if (allocated(this%stored)) deallocate (this%stored)

  end subroutine

  pure subroutine destroy_tablechar_t_scal(this)
   type(table_char_t), intent(inout) :: this

   call this%destroy()

  end subroutine

  pure subroutine destroy_tablechar_t_array1(this)
   type(table_char_t), intent(inout) :: this(:)

   integer :: i

   do i = 1, size(this)
    call this(i)%destroy()
   end do

  end subroutine

  pure function get_tablechar_t(this, index) result(c)
   class(table_char_t), intent(in) :: this
   integer(int32), intent(in) :: index
   character(len=len(this%stored)) :: c

   if (index .gt. this%filled) then
    return !what should be the flag
   end if

   c = this%stored(index)

  end function

  pure function getindex_tablechar_t(this, c) result(index)
   class(table_char_t), intent(in) :: this
   character(len=*), intent(in) :: c
   integer(int32) :: index

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     index = -2
     return
    else !occupied
     if (this%stored(this%id(address)) .eq. c) then
      index = this%id(address)
      return
     end if
    end if
   end do

   index = -1

  end function

  subroutine writetable_tablechar_t(this, namefile)
   class(table_char_t), intent(in) :: this
   character(*), intent(in) :: namefile

   integer :: i, un

   open (newunit=un, file=namefile, action='write')
   do i = 1, this%filled
    write (un, '(i10,1x,g0)') i, this%get(i)
   end do
   close (un)

  end subroutine

  pure function constructor_tableint32_t(nel) result(this)
   integer(kind=int32), intent(in), optional :: nel
   type(table_int32_t) :: this

   this%nel = roundinguppower2(default_nel)
   if (present(nel)) this%nel = roundinguppower2(nel)

   this%filled = 0

   allocate (this%id(this%nel))
   this%id = 0

   allocate (this%stored(this%nel))
   this%stored = 0

  end function

  recursive subroutine add_tableint32_t(this, c, index, lnew) !, col)
   class(table_int32_t), intent(inout) :: this
   integer(int32), intent(in) :: c
   integer(int32), intent(out), optional :: index
   !integer(int32), intent(out), optional :: col
   logical, intent(out), optional :: lnew

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (real(this%filled) .gt. 0.80*this%nel) call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = i
      return
     end if
    end if
   end do

   call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = min(this%nel, maxiter) - 1 + i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = min(this%nel, maxiter) - 1 + i
      return
     end if
    end if
   end do

   address = -1
   if (present(index)) index = -1

   error stop 'too many collisions'

    contains

     subroutine increase_size()
      type(table_int32_t) :: thistmp

       thistmp = table_int32_t(nel=int(this%nel*1.5, int32))

      do i = 1, this%filled
       call thistmp%add(this%stored(i))
      end do

      this%filled = thistmp%filled
      this%nel = thistmp%nel
      if (allocated(this%id)) deallocate (this%id)
      if (allocated(this%stored)) deallocate (this%stored)
      call move_alloc(thistmp%id, this%id)
      call move_alloc(thistmp%stored, this%stored)

     end subroutine

  end subroutine

  pure subroutine destroy_tableint32_t(this)
   class(table_int32_t), intent(inout) :: this

   call this%destroy_tableabstract_t

   if (allocated(this%stored)) deallocate (this%stored)

  end subroutine

  pure subroutine destroy_tableint32_t_scal(this)
   type(table_int32_t), intent(inout) :: this

   call this%destroy()

  end subroutine

  pure subroutine destroy_tableint32_t_array1(this)
   type(table_int32_t), intent(inout) :: this(:)

   integer :: i

   do i = 1, size(this)
    call this(i)%destroy()
   end do

  end subroutine

  pure function get_tableint32_t(this, index) result(c)
   class(table_int32_t), intent(in) :: this
   integer(int32), intent(in) :: index
   integer(int32) :: c

   if (index .gt. this%filled) then
    c = 0  !should NaN
    return !what should be the flag
   end if

   c = this%stored(index)

  end function

  pure function getindex_tableint32_t(this, c) result(index)
   class(table_int32_t), intent(in) :: this
   integer(int32), intent(in) :: c
   integer(int32) :: index

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     index = -2
     return
    else !occupied
     if (this%stored(this%id(address)) .eq. c) then
      index = this%id(address)
      return
     end if
    end if
   end do

   index = -1

  end function

  subroutine writetable_tableint32_t(this, namefile)
   class(table_int32_t), intent(in) :: this
   character(*), intent(in) :: namefile

   integer :: i, un

   open (newunit=un, file=namefile, action='write')
   do i = 1, this%filled
    write (un, '(i10,1x,g0)') i, this%get(i)
   end do
   close (un)

  end subroutine

  pure function constructor_tablereal32_t(nel) result(this)
   integer(kind=int32), intent(in), optional :: nel
   type(table_real32_t) :: this

   this%nel = roundinguppower2(default_nel)
   if (present(nel)) this%nel = roundinguppower2(nel)

   this%filled = 0

   allocate (this%id(this%nel))
   this%id = 0

   allocate (this%stored(this%nel))
   this%stored = 0

  end function

  recursive subroutine add_tablereal32_t(this, c, index, lnew) !, col)
   class(table_real32_t), intent(inout) :: this
   real(real32), intent(in) :: c
   integer(int32), intent(out), optional :: index
   !integer(int32), intent(out), optional :: col
   logical, intent(out), optional :: lnew

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (real(this%filled) .gt. 0.80*this%nel) call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = i
      return
     end if
    end if
   end do

   call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     !if (present(col)) col = min(this%nel, maxiter) - 1 + i
     return
    else !already occupied
     if (this%stored(this%id(address)) .eq. c) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      !if (present(col)) col = min(this%nel, maxiter) - 1 + i
      return
     end if
    end if
   end do

   address = -1
   if (present(index)) index = -1

   error stop 'too many collisions'

    contains

     subroutine increase_size()
      type(table_real32_t) :: thistmp

       thistmp = table_real32_t(nel=int(this%nel*1.5, int32))

      do i = 1, this%filled
       call thistmp%add(this%stored(i))
      end do

      this%filled = thistmp%filled
      this%nel = thistmp%nel
      if (allocated(this%id)) deallocate (this%id)
      if (allocated(this%stored)) deallocate (this%stored)
      call move_alloc(thistmp%id, this%id)
      call move_alloc(thistmp%stored, this%stored)

     end subroutine

  end subroutine

  pure subroutine destroy_tablereal32_t(this)
   class(table_real32_t), intent(inout) :: this

   call this%destroy_tableabstract_t

   if (allocated(this%stored)) deallocate (this%stored)

  end subroutine

  pure subroutine destroy_tablereal32_t_scal(this)
   type(table_real32_t), intent(inout) :: this

   call this%destroy()

  end subroutine

  pure subroutine destroy_tablereal32_t_array1(this)
   type(table_real32_t), intent(inout) :: this(:)

   integer :: i

   do i = 1, size(this)
    call this(i)%destroy()
   end do

  end subroutine

  pure function get_tablereal32_t(this, index) result(c)
   class(table_real32_t), intent(in) :: this
   integer(int32), intent(in) :: index
   real(real32) :: c

   if (index .gt. this%filled) then
    c = 0  !should NaN
    return !what should be the flag
   end if

   c = this%stored(index)

  end function

  pure function getindex_tablereal32_t(this, c) result(index)
   class(table_real32_t), intent(in) :: this
   real(real32), intent(in) :: c
   integer(int32) :: index

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     index = -2
     return
    else !occupied
     if (this%stored(this%id(address)) .eq. c) then
      index = this%id(address)
      return
     end if
    end if
   end do

   index = -1

  end function

  subroutine writetable_tablereal32_t(this, namefile)
   class(table_real32_t), intent(in) :: this
   character(*), intent(in) :: namefile

   integer :: i, un

   open (newunit=un, file=namefile, action='write')
   do i = 1, this%filled
    write (un, '(i10,1x,g0)') i, this%get(i)
   end do
   close (un)

  end subroutine


  pure function constructor_table_arrint32_t(k, nel) result(this)
   integer(kind=int32), intent(in) :: k
   integer(kind=int32), intent(in), optional :: nel
   type(table_arrint32_t) :: this

   this%nel = roundinguppower2(default_nel)
   if (present(nel)) this%nel = roundinguppower2(nel)

   this%filled = 0

   allocate (this%id(this%nel))
   this%id = 0

   allocate (this%stored(k, this%nel))
   this%stored = 0

  end function

  recursive subroutine add_table_arrint32_t(this, c, index, lnew)
   class(table_arrint32_t), intent(inout) :: this
   integer(int32), intent(in) :: c(:)
   integer(int32), intent(out), optional :: index
   logical, intent(out), optional :: lnew

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (size(c) .ne. size(this%stored, 1)) then
    if (present(index)) index = -1
    if (present(lnew)) lnew = .false.
    return
   end if

   if (real(this%filled) .gt. 0.80*this%nel) call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(:, this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     return
    else !already occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      return
     end if
    end if
   end do

   call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(:, this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     return
    else !already occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      return
     end if
    end if
   end do

   address = -1
   if (present(index)) index = -1

   error stop 'too many collisions'

  contains

   subroutine increase_size()
    type(table_arrint32_t) :: thistmp

    thistmp = table_arrint32_t(size(this%stored, 1), nel=int(this%nel*1.5, int32))

    do i = 1, this%filled
     call thistmp%add(this%stored(:, i))
    end do

    this%filled = thistmp%filled
    this%nel = thistmp%nel
    if (allocated(this%id)) deallocate (this%id)
    if (allocated(this%stored)) deallocate (this%stored)
    call move_alloc(thistmp%id, this%id)
    call move_alloc(thistmp%stored, this%stored)

   end subroutine

  end subroutine

  pure subroutine destroy_table_arrint32_t(this)
   class(table_arrint32_t), intent(inout) :: this

   call this%destroy_tableabstract_t

   if (allocated(this%stored)) deallocate (this%stored)

  end subroutine

  pure subroutine destroy_table_arrint32_t_scal(this)
   type(table_arrint32_t), intent(inout) :: this

   call this%destroy()

  end subroutine

  pure subroutine destroy_table_arrint32_t_array1(this)
   type(table_arrint32_t), intent(inout) :: this(:)

   integer :: i

   do i = 1, size(this)
    call this(i)%destroy()
   end do

  end subroutine

  pure function get_table_arrint32_t(this, index) result(c)
   class(table_arrint32_t), intent(in) :: this
   integer(int32), intent(in) :: index
   integer(int32) :: c(size(this%stored, 1))

   if (index .gt. this%filled) then
    c = 0
    return !what should be the flag
   end if

   c = this%stored(:, index)

  end function

  pure function getindex_table_arrint32_t(this, c) result(index)
   class(table_arrint32_t), intent(in) :: this
   integer(int32), intent(in) :: c(:)
   integer(int32) :: index

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (size(c) .ne. size(this%stored, 1)) then
    index = -1
    return
   end if

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     index = -1
     return
    else !occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      index = this%id(address)
      return
     end if
    end if
   end do

   index = -1

  end function

  subroutine writetable_table_arrint32_t(this, namefile)
   class(table_arrint32_t), intent(in) :: this
   character(*), intent(in) :: namefile

   integer :: i, un

   open (newunit=un, file=namefile, action='write')
   do i = 1, this%filled
    write (un, '(i10,*(1x,g0))') i, this%get(i)
   end do
   close (un)

  end subroutine

  pure function constructor_table_arrreal32_t(k, nel) result(this)
   integer(kind=int32), intent(in) :: k
   integer(kind=int32), intent(in), optional :: nel
   type(table_arrreal32_t) :: this

   this%nel = roundinguppower2(default_nel)
   if (present(nel)) this%nel = roundinguppower2(nel)

   this%filled = 0

   allocate (this%id(this%nel))
   this%id = 0

   allocate (this%stored(k, this%nel))
   this%stored = 0

  end function

  recursive subroutine add_table_arrreal32_t(this, c, index, lnew)
   class(table_arrreal32_t), intent(inout) :: this
   real(real32), intent(in) :: c(:)
   integer(int32), intent(out), optional :: index
   logical, intent(out), optional :: lnew

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (size(c) .ne. size(this%stored, 1)) then
    if (present(index)) index = -1
    if (present(lnew)) lnew = .false.
    return
   end if

   if (real(this%filled) .gt. 0.80*this%nel) call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(:, this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     return
    else !already occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      return
     end if
    end if
   end do

   call increase_size()

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     this%filled = this%filled + 1
     this%id(address) = this%filled
     this%stored(:, this%id(address)) = c
     if (present(index)) index = this%id(address)
     if (present(lnew)) lnew = .true.
     return
    else !already occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      if (present(index)) index = this%id(address)
      if (present(lnew)) lnew = .false.
      return
     end if
    end if
   end do

   address = -1
   if (present(index)) index = -1

   error stop 'too many collisions'

  contains

   subroutine increase_size()
    type(table_arrreal32_t) :: thistmp

    thistmp = table_arrreal32_t(size(this%stored, 1), nel=int(this%nel*1.5, int32))

    do i = 1, this%filled
     call thistmp%add(this%stored(:, i))
    end do

    this%filled = thistmp%filled
    this%nel = thistmp%nel
    if (allocated(this%id)) deallocate (this%id)
    if (allocated(this%stored)) deallocate (this%stored)
    call move_alloc(thistmp%id, this%id)
    call move_alloc(thistmp%stored, this%stored)

   end subroutine

  end subroutine

  pure subroutine destroy_table_arrreal32_t(this)
   class(table_arrreal32_t), intent(inout) :: this

   call this%destroy_tableabstract_t

   if (allocated(this%stored)) deallocate (this%stored)

  end subroutine

  pure subroutine destroy_table_arrreal32_t_scal(this)
   type(table_arrreal32_t), intent(inout) :: this

   call this%destroy()

  end subroutine

  pure subroutine destroy_table_arrreal32_t_array1(this)
   type(table_arrreal32_t), intent(inout) :: this(:)

   integer :: i

   do i = 1, size(this)
    call this(i)%destroy()
   end do

  end subroutine

  pure function get_table_arrreal32_t(this, index) result(c)
   class(table_arrreal32_t), intent(in) :: this
   integer(int32), intent(in) :: index
   real(real32) :: c(size(this%stored, 1))

   if (index .gt. this%filled) then
    c = 0
    return !what should be the flag
   end if

   c = this%stored(:, index)

  end function

  pure function getindex_table_arrreal32_t(this, c) result(index)
   class(table_arrreal32_t), intent(in) :: this
   real(real32), intent(in) :: c(:)
   integer(int32) :: index

   integer(int32) :: i, address
   integer(int32), parameter :: maxiter = 5000

   if (size(c) .ne. size(this%stored, 1)) then
    index = -1
    return
   end if

   address = seed_hash - 1

   do i = 1, min(this%nel, maxiter) - 1
    address = iand(hash(c, address + i), this%nel - 1) + 1

    if (this%id(address) .eq. 0) then
     index = -1
     return
    else !occupied
     if (all(this%stored(:, this%id(address)) .eq. c)) then
      index = this%id(address)
      return
     end if
    end if
   end do

   index = -1

  end function

  subroutine writetable_table_arrreal32_t(this, namefile)
   class(table_arrreal32_t), intent(in) :: this
   character(*), intent(in) :: namefile

   integer :: i, un

   open (newunit=un, file=namefile, action='write')
   do i = 1, this%filled
    write (un, '(i10,*(1x,g0))') i, this%get(i)
   end do
   close (un)

  end subroutine


  end module
