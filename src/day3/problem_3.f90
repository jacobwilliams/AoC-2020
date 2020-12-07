program problem_3

!! how many trees would you encounter?

use aoc_utilities

implicit none

integer :: iunit, istat, n_lines, i, j, n_cols, n_trees, col
character(len=1000) :: line
logical,dimension(:,:),allocatable :: data

! read into a logical array (true=true)
open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    read(iunit, '(A1000)', iostat=istat) line
    if (is_iostat_end(istat)) exit
    if (i==1) then
        n_cols = len_trim(line)
        allocate(data(n_lines, n_cols))
    end if
    do j = 1, n_cols
        data(i,j) = line(j:j) == '#'
    end do
end do

col = 0
n_trees = 0
do i = 1, n_lines
    if (data(i,col+1)) n_trees = n_trees + 1
    col = mod(col + 3, n_cols)
end do

write(*,*) 'n_trees = ', n_trees

end program problem_3