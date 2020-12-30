program problem_12b

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
    integer,dimension(2) :: waypoint ! waypoint (x,y) position
    
    open(newunit=iunit, file='input.txt', iostat=istat)
    n = number_of_lines_in_file(iunit)
    
    r = 0               ! initial position
    waypoint = [10,1]   ! 10 units east and 1 unit north
    
    do i = 1, n
        call read_line_from_file(iunit,chunk_size,line,status_ok)
    
        action = line(1:1)
        read(line(2:),'(I5)') value

        select case (action)
        case('N'); waypoint(2) = waypoint(2) + value  ! move waypoint north by the given value.    
        case('S'); waypoint(2) = waypoint(2) - value  ! move waypoint south by the given value.
        case('E'); waypoint(1) = waypoint(1) + value  ! move waypoint east by the given value.
        case('W'); waypoint(1) = waypoint(1) - value  ! move waypoint west by the given value.
        case('F'); r = r + waypoint * value  ! move forward to the waypoint a number of times equal to the given value
        
        ! L: rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
        ! R: rotate the waypoint around the ship right (clockwise) the given number of degrees.
        case('L', 'R')
            if (action=='R' .and. value /= 180) value = wrap(value + 180)
            select case (value)
            case(90);  waypoint = [-waypoint(2), waypoint(1)]
            case(180); waypoint = [-waypoint(1), -waypoint(2)]
            case(270); waypoint = [waypoint(2), -waypoint(1)]
            case default
                write(*,*) value
                error stop 'invalid rotation'
            end select
        case default
            error stop 'invalid action: '//action
        end select 
    
    end do
    
    write(*,*) 'Manhattan distance: ', abs(r(1)) + abs(r(2))
    
    contains
    
    pure function wrap(angle) result(wrapped_angle)
    !! wrap from 0 -> 360
    implicit none 
    integer,intent(in) :: angle
    integer :: wrapped_angle
    wrapped_angle = mod(angle, 360)
    if (wrapped_angle<0) wrapped_angle = wrapped_angle + 360
    end function wrap
    
    end program problem_12b