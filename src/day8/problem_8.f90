program problem_8

!! what value is in the accumulator?

use aoc_utilities
use iso_fortran_env

implicit none

integer :: iunit, istat, n_lines, i, j, k, accumulator
character(len=:),allocatable :: line, tmp
logical :: status_ok
type(string),dimension(:),allocatable :: operation_argument

integer,parameter :: chunk_size = 256

type :: instruction
    character(len=:),allocatable :: operation
    integer :: argument = 0
    logical :: done = .false. !! if this one has already been done
end type instruction
type(instruction),dimension(:),allocatable :: instructions

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)
allocate(instructions(n_lines))

! read the instructions:
do i = 1, n_lines
    call read_line_from_file(iunit,chunk_size,line,status_ok)
    call split(line,' ',chunk_size,operation_argument)
    instructions(i)%operation = trim(adjustl(operation_argument(1)%str))
    tmp = trim(adjustl(operation_argument(2)%str))
    read(tmp, '(I10)') instructions(i)%argument   ! string to int
    instructions(i)%done = .false.
end do
write(*,*) 'number of instructions: ', n_lines

! do i = 1, n_lines
!     write(*,*) instructions(i)%operation, instructions(i)%argument
! end do
! write(*,*) ''

! now process each one:
accumulator = 0
i = 1  ! start with first one
do

    !write(*,*) i, instructions(i)%operation, instructions(i)%argument, instructions(i)%done, accumulator

    if (instructions(i)%done) then
        write(*,*) 'instruction ', i, ' has already been done'
        write(*,*) 'accumulator = ', accumulator
        exit
    end if

    instructions(i)%done = .true.

    select case (instructions(i)%operation)
    case('acc')
        accumulator = accumulator + instructions(i)%argument
        i = i + 1
    case('jmp')
        i = i + instructions(i)%argument
    case('nop')
        i = i + 1
    case default
        error stop 'error: unknown operation: '//instructions(i)%operation
    end select

end do

end program problem_8