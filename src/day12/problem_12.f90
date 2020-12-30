program problem_12

!! What is the Manhattan distance between that location 
!! and the ship's starting position?

use aoc_utilities
use iso_fortran_env

implicit none

integer,parameter :: chunk_size = 256

integer :: n, iunit, istat, i
character(len=:),allocatable :: line
logical :: status_ok
character(len=1) :: action
integer :: value
integer,dimension(2) :: r ! (x,y) position
integer :: direction ! current direction (N=90,S=270,E=0,W=180)

open(newunit=iunit, file='input.txt', iostat=istat)
n = number_of_lines_in_file(iunit)

direction = 0 ! start facing east
r = 0         ! initial position

do i = 1, n
    call read_line_from_file(iunit,chunk_size,line,status_ok)

    action = line(1:1)
    read(line(2:),'(I5)') value

    select case (action)
    case('N'); r(2) = r(2) + value  ! move north by the given value.    
    case('S'); r(2) = r(2) - value  ! move south by the given value.
    case('E'); r(1) = r(1) + value  ! move east by the given value.
    case('W'); r(1) = r(1) - value  ! move west by the given value.
    case('L'); direction = wrap(direction + value)  ! turn left the given number of degrees.
    case('R'); direction = wrap(direction - value)  ! turn right the given number of degrees.
    case('F')  ! move forward by the given value in the direction the ship is currently facing.
        select case (direction)
        case(0,360); r(1) = r(1) + value  ! E
        case(90);    r(2) = r(2) + value  ! N
        case(180);   r(1) = r(1) - value  ! W
        case(270);   r(2) = r(2) - value  ! S
        case default  
            error stop 'invalid direction'
        end select
    case default
        error stop 'invalid action: '//action
    end select 

end do

write(*,*) 'Manhattan distance: ', abs(r(1)) + abs(r(2))

contains

pure function wrap(angle) result(wrapped_angle)
implicit none 
integer,intent(in) :: angle
integer :: wrapped_angle
wrapped_angle = mod(angle, 360)
if (wrapped_angle<0) wrapped_angle = wrapped_angle + 360
end function wrap

end program problem_12