module aoc_utilities

    use iso_fortran_env

    implicit none

    private

    public :: read_file_to_integer_array
    public :: number_of_lines_in_file
    public :: sort_ascending

contains

!****************************************************************
    function read_file_to_integer_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer,dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, '(I10)') iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer_array
!****************************************************************

!****************************************************************
    function number_of_lines_in_file(iunit) result(n_lines)

    implicit none

    integer,intent(in)  :: iunit   !! the file unit number
                                    !! (assumed to be open)
    integer :: n_lines   !! the number of lines in the file

    character(len=1) :: tmp
    integer :: istat

    rewind(iunit)
    n_lines = 0
    do
        read(iunit,fmt='(A1)',iostat=istat) tmp
        if (is_iostat_end(istat)) exit
        n_lines = n_lines + 1
    end do
    rewind(iunit)

    end function number_of_lines_in_file
!****************************************************************

!*******************************************************************************
!>
!  Sorts an integer array `ivec` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    implicit none

    integer,dimension(:),intent(inout) :: ivec

    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1,size(ivec))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        implicit none

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap(ivec(ip),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending
!*******************************************************************************

!*******************************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap(i1,i2)

    implicit none

    integer,intent(inout) :: i1
    integer,intent(inout) :: i2

    integer :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap
!*******************************************************************************


end module aoc_utilities