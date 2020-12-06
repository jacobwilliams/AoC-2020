module aoc_utilities

use iso_fortran_env

implicit none

private

public :: read_file_to_integer_array
public :: number_of_lines_in_file

contains

!****************************************************************
function read_file_to_integer_array(filename) result(iarray)

character(len=*),intent(in) :: filename
integer,dimension(:),allocatable :: iarray

integer :: i, iunit, n_lines, istat

open(newunit=iunit, file=filename, iostat=istat)
if (istat /= 0) error stop ' error reading file'

n_lines = number_of_lines_in_file(iunit)
allocate(iarray(n_lines))
do i = 1, n_lines
    read(iunit, '(I10)') iarray(i)
end do

close(iunit)

end function read_file_to_integer_array
!****************************************************************

!****************************************************************
function number_of_lines_in_file(iunit) result(n_lines)

implicit none

integer,intent(in)  :: iunit   !! the file unit number
                                !! (assumed to be open)
integer :: n_lines   !! the number of lines in the file

character(len=1) :: tmp
integer :: istat

rewind(iunit)
n_lines = 0
do
    read(iunit,fmt='(A1)',iostat=istat) tmp
    if (is_iostat_end(istat)) exit
    n_lines = n_lines + 1
end do
rewind(iunit)

end function number_of_lines_in_file
!****************************************************************

end module aoc_utilities