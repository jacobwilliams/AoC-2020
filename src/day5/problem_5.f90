program problem_5

!! What is the highest seat ID on a boarding pass?

use aoc_utilities
use iso_fortran_env

implicit none

integer :: iunit, istat, n_lines, i, istart, iend, j, irow, icol, id
character(len=10) :: row
integer,dimension(:),allocatable :: ids

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)
allocate(ids(n_lines))

do i = 1, n_lines
    read(iunit, '(A10)') row

    istart = 0
    iend   = 127
    irow = find(row(1:7), istart, iend, 'FB')

    istart = 0
    iend   = 7
    icol = find(row(8:10), istart, iend, 'LR')

    id = irow * 8 + icol
    ids(i) = id

    !write(*,*) irow, icol, id

end do

write(*,*) 'max id: ', maxval(ids)

! part b:
write(*,*) ''
call sort_ascending(ids)
do i = 2, n_lines
    if (ids(i) - ids(i-1) == 2) then
        write(*,*) 'seat id: ', ids(i) - 1
        stop
    end if
end do

contains
    function find(row, istart, iend, lower_upper) result(irow)

    implicit none

    character(len=*),intent(in) :: row
    integer,intent(in) :: istart, iend
    character(len=2),intent(in) :: lower_upper
    integer :: irow

    integer :: istart_, iend_
    istart_ = istart
    iend_   = iend
    do j = 1, len(row)
        if (row(j:j) == lower_upper(1:1)) then ! lower
            iend_ = istart_ + (iend_-istart_-1)/2
        elseif (row(j:j) == lower_upper(2:2)) then ! upper
            istart_ = istart_ + (iend_-istart_-1)/2 + 1
        else
            error stop 'invalid char: '//row(j:j)
        end if
    end do
    irow = istart_
    end function find

end program problem_5