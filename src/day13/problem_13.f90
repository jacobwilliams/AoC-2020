program problem_13

!! What is the ID of the earliest bus you can take to 
!! the airport multiplied by the number of minutes you'll 
!! need to wait for that bus?

use aoc_utilities
use iso_fortran_env, ip => int32

implicit none

integer,parameter :: chunk_size = 256

integer :: n, iunit, istat, i, j, best_bus, bus
character(len=:),allocatable :: line
logical :: status_ok
integer(ip) :: earliest_timestamp, timestamp, best_timestamp, wait_time
type(string),dimension(:),allocatable :: vals

open(newunit=iunit, file='input.txt', iostat=istat)

call read_line_from_file(iunit,chunk_size,line,status_ok)
read(line,'(I10)') earliest_timestamp

call read_line_from_file(iunit,chunk_size,line,status_ok)
call split(line,',',chunk_size,vals)

n = size(vals)
best_bus = 0                 ! closest so far to earliest_timestamp
best_timestamp = huge(1_ip)  !

do i = 1, n
    if (vals(i)%str == 'x') cycle
    read(vals(i)%str, '(I10)') bus

    timestamp = 0
    do
        timestamp = timestamp + bus
        if (timestamp >= earliest_timestamp) then
            write(*,*) 'for bus ', bus, ' : ', timestamp
            if (timestamp<best_timestamp) then
                best_timestamp = timestamp
                best_bus = bus
            end if
            exit
        end if
    end do

end do

wait_time = best_timestamp - earliest_timestamp

write(*,*) ''
write(*,*) 'best bus:       ', best_bus 
write(*,*) 'best timestamp: ', best_timestamp
write(*,*) 'wait time:      ', best_timestamp - earliest_timestamp
write(*,*) 'ID * wait time: ', best_bus * wait_time

end program problem_13