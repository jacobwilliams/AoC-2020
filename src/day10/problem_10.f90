program problem_10

!! What is the first number that does not have this property?

use aoc_utilities, only: read_file_to_integer_array, sort_ascending
use iso_fortran_env

implicit none

integer,dimension(:),allocatable :: output_joltage
integer :: i, j, k, n, highest_rated, device_adapter_voltage, diff, diff_1v, diff_3v

output_joltage = read_file_to_integer_array('input.txt')
highest_rated = maxval(output_joltage)
device_adapter_voltage = highest_rated + 3
output_joltage = [output_joltage, device_adapter_voltage]
call sort_ascending(output_joltage)
n = size(output_joltage)

diff_1v = 0
diff_3v = 0

do i = 1, n
    if (i==1) then
        diff = output_joltage(i)
    else
        diff = output_joltage(i) - output_joltage(i-1)
    end if
    if (i>1 .and. .not. any(diff==[1,2,3])) error stop 'error'  ! verify
    select case(diff)
    case(1); diff_1v = diff_1v + 1
    case(3); diff_3v = diff_3v + 1
    end select
end do

write(*,*) '1 v diffs: ', diff_1v
write(*,*) '3 v diffs: ', diff_3v
write(*,*) 'product  : ', diff_1v * diff_3v

end program problem_10