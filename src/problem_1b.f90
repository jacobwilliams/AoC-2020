program problem_1b

!! what is the product of the three entries that sum to 2020?

use aoc_utilities

implicit none

integer,dimension(:),allocatable :: iarray
integer :: i, j, k, n, tmp

iarray = read_file_to_integer_array('input.txt')
call sort_ascending(iarray) ! sort so we can short circuit the loop when the sum gets too big
n = size(iarray)

do i = 1, n
    if (iarray(i)>2020) exit
    do j = 1, n
        if (i==j) cycle
        tmp = iarray(i) + iarray(j)
        if (tmp>2020) exit
        do k = 1, n
            if (i==k .or. j==k) cycle
            tmp = tmp + iarray(k)
            if (tmp==2020) then
                write(*,*) iarray(i) , '+', iarray(j), '+', iarray(k), '=', sum(iarray([i,j,k]))
                write(*,*) iarray(i) , '*', iarray(j), '*', iarray(k), '=', product(iarray([i,j,k]))
                stop
            end if
            if (tmp>2020) exit
        end do
    end do
end do

end program problem_1b