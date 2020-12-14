program problem_9

!! What is the first number that does not have this property?

use aoc_utilities, only: read_file_to_integer64_array
use iso_fortran_env

implicit none

integer(int64),dimension(:),allocatable :: iarray
integer :: i, j, k, n
logical :: ok
integer(int64) :: invalid_value, smallest, largest, weakness

iarray = read_file_to_integer64_array('input.txt')
n = size(iarray)

main: do i = 26, n
    write(*,*) 'try: ', i
    ok = .false.
    loop: do j = 1, n-2
        do k = j+1, n-1
            if (iarray(i) == iarray(j) + iarray(k)) then
                ok = .true.
                exit loop
            end if
        end do
    end do loop
    if (.not. ok) then
        invalid_value = iarray(i)
        write(*,*) 'found: entry ', i, ' value = ', invalid_value
        exit main
    end if

end do main

! What is the encryption weakness in your XMAS-encrypted list of numbers?

write(*,*) ''
write(*,*) 'Part 2'
write(*,*) ''

do i = 1, n-1
    do j = 1, n
        if (sum(iarray(i:j)) == invalid_value) then
            write(*,*) 'found sequence: ', iarray(i:j)
            smallest = minval(iarray(i:j))
            largest  = maxval(iarray(i:j))
            weakness = smallest + largest
            write(*,*) 'weakness: ', weakness
            stop
        end if
    end do
end do


end program problem_9