program problem_1

!! Find the two entries that sum to 2020;
!! what do you get if you multiply them together?

use aoc_utilities, only: read_file_to_integer_array

implicit none

integer,dimension(:),allocatable :: iarray
integer :: i, j

iarray = read_file_to_integer_array('input.txt')

do i = 1, size(iarray)
    do j = i+1, size(iarray)
        if (iarray(i) + iarray(j) == 2020) then
            write(*,*) iarray(i) , '*', iarray(j), iarray(i) * iarray(j)
            stop
        end if
    end do
end do

end program problem_1