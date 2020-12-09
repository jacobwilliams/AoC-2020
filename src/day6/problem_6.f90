program problem_6

!! What is the sum of those counts?

use aoc_utilities
use iso_fortran_env

implicit none

integer :: iunit, istat, n_lines, i, j, count_sum
character(len=:),allocatable :: line
logical :: status_ok
character(len=:),allocatable :: record

integer,parameter :: chunk_size = 256

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)

count_sum = 0
record = ''
do i = 1, n_lines
    call read_line_from_file(iunit,chunk_size,line,status_ok)
    if (line == '') then
        ! process this record:
        count_sum = count_sum + size(unique([(ichar(record(j:j)), j = 1, len(record))]))
        record = '' ! reset for next
    else
        ! accumulate this record:
        record = record // line
    end if
end do

! last one:
count_sum = count_sum + size(unique([(ichar(record(j:j)), j = 1, len(record))]))

write(*,*) 'count_sum = ' , count_sum

end program problem_6