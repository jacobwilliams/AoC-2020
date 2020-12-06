program problem_2

!! How many passwords are valid according to their policies?

implicit none

character(len=1000) :: line
character(len=:),allocatable :: password
character(len=1),dimension(:),allocatable :: ichars
character(len=1) :: letter
integer :: iunit, istat, s, d, c, imin, imax, n_letters, good, good_b

open(newunit=iunit, file='input.txt')
good = 0   ! part a
good_b = 0 ! part b
do
    read(iunit, '(A1000)', iostat=istat) line
    if (is_iostat_end(istat)) exit

    d = index(line, '-')
    s = index(line, ' ')
    c = index(line, ':')

    read(line(1:d-1),'(I5)')   imin
    read(line(d+1:s-1),'(I5)') imax
    letter = line(s+1:s+1)
    password = trim(adjustl(line(c+1:)))

    ! part 1
    n_letters = count( letter == transfer(password, ichars) )
    if (n_letters>=imin .and. n_letters<=imax) good = good + 1

    ! part 2
    if ((password(imin:imin)==letter .or. &
         password(imax:imax)==letter) .and. &
         password(imin:imin)/=password(imax:imax)) good_b = good_b + 1

end do

write(*,*) 'good (part 1)= ', good
write(*,*) 'good (part 2)= ', good_b

end program problem_2