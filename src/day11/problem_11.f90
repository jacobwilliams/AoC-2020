program problem_11

!! How many seats end up occupied?

use aoc_utilities
use iso_fortran_env

implicit none

integer,dimension(:,:),allocatable :: seat_array, prev_array, new_array
integer :: iunit, istat, i, j, k, n, m, iter
character(len=:),allocatable :: line
logical :: status_ok

integer,parameter :: chunk_size = 256

! read the file:
open(newunit=iunit, file='input.txt', iostat=istat)
n = number_of_lines_in_file(iunit)
do i = 1, n
    call read_line_from_file(iunit,chunk_size,line,status_ok)
    if (.not. allocated(seat_array)) then
        m = len(line)
        allocate(seat_array(n, m))
    end if
    ! in the seat array:
    ! -1 = floor (.)
    ! 0 = empty (L)
    ! 1 = occupied (#)
    do j = 1, m
        select case (line(j:j))
        case('.'); seat_array(i,j) = -1
        case('L'); seat_array(i,j) = 0
        case('#'); seat_array(i,j) = 1
        case default; error stop 'invalid value: '//line(j:j)
        end select
    end do
end do

write(*,*) '--'
call print_array(seat_array)

prev_array = seat_array
iter = 0
do
    iter = iter + 1
    ! If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    ! If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    ! Otherwise, the seat's state does not change.
    do i = 1, n
        do j = 1, m
            select case (prev_array(i,j))
            case(0) ! empty
                ! check for any occupied adjacent seats
                if (number_of_occupied_adjacent_sets(i, j, prev_array)==0) seat_array(i,j) = 1
            case(1) ! occupied
                if (number_of_occupied_adjacent_sets(i, j, prev_array)>=4) seat_array(i,j) = 0
            end select
        end do
    end do

    write(*,*) '--'
    call print_array(seat_array)

    ! has it changed:
    if (all(prev_array == seat_array)) then
        write(*,*) 'done in ', iter, 'iterations'
        exit
    else
        prev_array = seat_array
    end if

end do

write(*,*) 'number occupied: ', count(seat_array==1)

contains

    function number_of_occupied_adjacent_sets(i, j, iarray) result(n_occupied)
    implicit none
    integer :: i, j !! Index to check
    integer,dimension(:,:),intent(in) :: iarray
    integer :: n_occupied, ii, jj

    integer :: n, m

    n = size(iarray,1)
    m = size(iarray,2)

    n_occupied = 0

    ! ###
    ! #x#
    ! ###

    do ii = i-1, i+1
        do jj = j-1, j+1
            if (ii==i .and. jj==j) cycle
            if (ii>0 .and. ii<=n .and. jj>0 .and. jj<=m) then
                if (iarray(ii,jj)==1) n_occupied = n_occupied + 1
            end if
        end do
    end do

    end function number_of_occupied_adjacent_sets

    subroutine print_array(iarray)
    implicit none
    integer,dimension(:,:),intent(in) :: iarray
    integer :: i, j

    do i = 1, size(iarray,1)
        do j = 1, size(iarray, 2)
            select case (iarray(i,j))
            case(-1); write(*,'(A)', advance='NO') '.'
            case(0);  write(*,'(A)', advance='NO') 'L'
            case(1);  write(*,'(A)', advance='NO') '#'
            end select
        end do
        write(*,'(A)') ''
    end do

    end subroutine print_array

end program problem_11