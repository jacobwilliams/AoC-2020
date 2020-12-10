program problem_7

!! How many bag colors can eventually contain at least one shiny gold bag?

use aoc_utilities
use iso_fortran_env

implicit none

type :: bag
    integer :: id = 0
    character(len=:),allocatable :: name
    integer,dimension(:),allocatable :: contents ! bags containied within (ids)
    character(len=:),allocatable :: tmp
end type bag

type(bag),dimension(:),allocatable :: bags
integer :: iunit, istat, n_lines, i, j, k, bag_count
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: bag_contents
logical :: found
integer :: index_to_find

integer,parameter :: chunk_size = 256
character(len=*),parameter :: bag_to_find = 'shiny gold'

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)

allocate(bags(n_lines))

! first get all the bags:
index_to_find = 0
do i = 1, n_lines
    call read_line_from_file(iunit,chunk_size,line,status_ok)
    call split(line,'bags contain',chunk_size,bag_contents)
    bags(i)%id = i
    bags(i)%name = trim(adjustl(bag_contents(1)%str))
    allocate(bags(i)%contents(0))
    bags(i)%tmp = trim(adjustl(bag_contents(2)%str))  ! save for later
    if (bags(i)%name == bag_to_find) index_to_find = i ! save this index
end do
write(*,*) 'number of bags: ', n_lines

! now process the contents of each bag:
do i = 1, n_lines
    if (bags(i)%tmp /= 'no other bags.') then
        call split(bags(i)%tmp,',',chunk_size,bag_contents)
        do j = 1, size(bag_contents)
            found = .false.
            ! find the index of this bag
            do k = 1, n_lines
                if (index(bag_contents(j)%str, bags(k)%name)>0) then
                    found = .true.
                    bags(i)%contents = [bags(i)%contents, k]
                    exit
                end if
            end do
            if (.not. found) error stop 'invalid contents: '//bag_contents(j)%str
        end do
    end if
end do

! now, find all outermost bags that can contain at least one shiny gold bag
bag_count = 0
do i = 1, n_lines
    if (i==index_to_find) cycle

    found = .false.
    call find_bag(i, found)
    if (found) then
        !write(*,*) 'valid outermost bag: '//bags(i)%name
        bag_count = bag_count + 1
    end if
end do

write(*,*) 'number of valid outermost bags: ', bag_count

contains

    recursive subroutine find_bag(bag_id, found)
    implicit none
    integer,intent(in) :: bag_id
    logical,intent(inout) :: found
    integer :: i
    if (found) return
    if (bag_id==index_to_find) then
        found = .true.
        return
    end if
    if (size(bags(bag_id)%contents)>0) then
        do i = 1, size(bags(bag_id)%contents)
            call find_bag(bags(bag_id)%contents(i), found)
            if (found) return
        end do
    end if
    end subroutine find_bag

end program problem_7