program problem_10

!! What is the first number that does not have this property?

use aoc_utilities, only: read_file_to_integer_array, sort_ascending
use iso_fortran_env

implicit none

integer,dimension(:),allocatable :: output_joltage
integer :: i, j, k, n, highest_rated, device_adapter_voltage, diff, diff2, diff_1v, diff_3v
integer :: total_valid, istart, iend, n_valid
logical,dimension(:),allocatable :: removable

type :: sequence
    integer,dimension(:),allocatable :: i
    integer :: n_valid = 0
end type sequence
type(sequence),allocatable,dimension(:) :: sequences

output_joltage = read_file_to_integer_array('input.txt')
highest_rated = maxval(output_joltage)
device_adapter_voltage = highest_rated + 3
output_joltage = [0, output_joltage, device_adapter_voltage]
call sort_ascending(output_joltage)
n = size(output_joltage)

do i=1,n
    write(*,*) output_joltage(i)
end do
write(*,*) ''
write(*,*) n, 'elements'
write(*,*) ''

diff_1v = 0
diff_3v = 0

do i = 2, n
    diff = output_joltage(i) - output_joltage(i-1)
    if (.not. any(diff==[1,2,3])) error stop 'error'  ! verify
    select case(diff)
    case(1); diff_1v = diff_1v + 1
    case(3); diff_3v = diff_3v + 1
    end select
end do

write(*,*) '1 v diffs: ', diff_1v
write(*,*) '3 v diffs: ', diff_3v
write(*,*) 'product  : ', diff_1v * diff_3v

write(*,*) ''
write(*,*) '=========================='
write(*,*) '         Part 2'
write(*,*) '=========================='
write(*,*) ''

! identify candidates for removal:
allocate(removable(n))
removable = .false.
do i = 2, n-1
    diff = output_joltage(i+1) - output_joltage(i-1)
    if (any(diff==[1,2,3])) then
        removable(i) = .true. ! this one could be removed
        !write(*,'(A,1X,I3,1X,A,I2,A)') 'index ', i, '(', output_joltage(i),') is removable'
    end if
end do
! write(*,*) count(removable), 'are removable'

! write(*,*) ''
! write(*,*) 'removable: ', removable
! write(*,*) ''

! get consecutive sequences...these are the ones that have to be checked:
allocate(sequences(0))
istart = 0
iend = 2
do i = 2, n-1

    if (removable(i)) then
        if (istart==0) then
            istart = i  ! first one
        end if
        iend = i  ! add another one
    else
        if (istart /= 0) then
            ! write(*,*) 'sequence: ', [(j, j = istart, iend)]
            sequences = [ sequences, sequence([(j, j = istart, iend)]) ]
        end if
        istart = 0
        iend = 0
    end if

end do

! write(*,*) '....sequences:'
! do i=1, size(sequences)
!     write(*,*) sequences(i)%i
! end do

! 1     -> 1
! 1,2   -> 1, 2,    12,                 2^2-1
! 1,2,3 -> 1, 2, 3, 12, 13, 23, 123     2^3-1
!
!... only have to check the conseq ones, the single ones are valid
! .. so, have to check 12, 13, 23, 123

! write(*,*) ''
! write(*,*) '----- get combos -----'
! write(*,*) ''

do i=1, size(sequences)
    !write(*,*) sequences(i)%i
    ! do j = 1, size(sequences(i)%i)
    !     write(*,*) sequences(i)%i(j)
    ! end do
    sequences(i)%n_valid = sequences(i)%n_valid + size(sequences(i)%i)  ! we can also remove them individually
    select case (size(sequences(i)%i))
    case(1)
        ! already counted this
    case(2)
        ! can we remove both at the same time?
        diff = output_joltage(sequences(i)%i(2)+1) - output_joltage(sequences(i)%i(1)-1)
        if (any(diff==[1,2,3])) then
            n_valid = n_valid + 1
            !write(*,*) sequences(i)%i(1), sequences(i)%i(2)
            sequences(i)%n_valid = sequences(i)%n_valid + 1
        end if
    case(3)
        ! remove 1-2
        diff = output_joltage(sequences(i)%i(2)+1) - output_joltage(sequences(i)%i(1)-1)
        if (any(diff==[1,2,3])) then
            n_valid = n_valid + 1
            !write(*,*) sequences(i)%i(1), sequences(i)%i(2)
            sequences(i)%n_valid = sequences(i)%n_valid + 1
        end if
        ! remove 1-3
        diff = output_joltage(sequences(i)%i(2))   - output_joltage(sequences(i)%i(1)-1)
        diff2 = output_joltage(sequences(i)%i(3)+1) - output_joltage(sequences(i)%i(2))
        if (any(diff==[1,2,3]) .and. any(diff2==[1,2,3])) then
            n_valid = n_valid + 1
            !write(*,*) sequences(i)%i(1), sequences(i)%i(3)
            sequences(i)%n_valid = sequences(i)%n_valid + 1
        end if
        ! remove 2-3
        diff = output_joltage(sequences(i)%i(3)+1) - output_joltage(sequences(i)%i(2)-1)
        if (any(diff==[1,2,3])) then
            n_valid = n_valid + 1
            !write(*,*) sequences(i)%i(2), sequences(i)%i(3)
            sequences(i)%n_valid = sequences(i)%n_valid + 1
        end if
        ! remove 1-2-3     !---> can never happen ...
        diff = output_joltage(sequences(i)%i(3)+1) - output_joltage(sequences(i)%i(1)-1)
        if (any(diff==[1,2,3])) then
            n_valid = n_valid + 1
           !write(*,*) sequences(i)%i(1), sequences(i)%i(2), sequences(i)%i(3)
            sequences(i)%n_valid = sequences(i)%n_valid + 1
        end if

    case default
        error stop 'oops have not accounted for this'
    end select
end do

write(*,*) ''
write(*,*) 'number of distinct ways: ', product(int(sequences%n_valid+1,int64))
write(*,*) ''

end program problem_10