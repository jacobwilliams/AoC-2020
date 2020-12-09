program problem_6

!! What is the sum of those counts?

use aoc_utilities
use iso_fortran_env

implicit none

integer :: iunit, istat, n_lines, i, j, count_sum, count_sum_b, n_people
character(len=:),allocatable :: line
logical :: status_ok
character(len=:),allocatable :: record

integer,parameter :: chunk_size = 256

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)

count_sum = 0
n_people = 0
record = ''
count_sum_b = 0
do i = 1, n_lines
    call read_line_from_file(iunit,chunk_size,line,status_ok)
    if (line == '') then
        ! process this record:
        count_sum   = count_sum   + size(unique([(ichar(record(j:j)), j = 1, len(record))]))
        count_sum_b = count_sum_b + all_yes(record, n_people)
        record = '' ! reset for next
        n_people = 0
    else
        ! accumulate this record:
        record = record // line
        n_people = n_people + 1
    end if
end do

! last one:
count_sum   = count_sum   + size(unique([(ichar(record(j:j)), j = 1, len(record))]))
count_sum_b = count_sum_b + all_yes(record, n_people)

write(*,*) 'count_sum a (any yes) = ' , count_sum
write(*,*) 'count_sum b (all yes) = ' , count_sum_b

contains

    function all_yes(record, n_people)

    implicit none

    character(len=*),intent(in) :: record
    integer,intent(in) :: n_people
    integer :: all_yes

    integer :: j, k, n
    integer,dimension(26) :: questions
    integer,dimension(:),allocatable :: irecord

    all_yes = 0
    questions = ichar('a') + [(i, i = 0, 25)]
    n = len(record)
    irecord = [(ichar(record(j:j)), j = 1,n)]
    do k = 1, 26
        if (count(questions(k)==irecord) == n_people) then
            all_yes = all_yes + 1
        end if
    end do

    end function all_yes

end program problem_6