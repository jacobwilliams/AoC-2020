program problem_4

!! In your batch file, how many passports are valid?

use aoc_utilities
use iso_fortran_env

implicit none

integer,parameter :: chunk_size = 256

integer :: iunit, istat, n_lines, record_num, i, j, n_valid
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: key_vals, key_val

type :: record
    logical :: byr = .false.  ! (Birth Year)
    logical :: iyr = .false.  ! (Issue Year)
    logical :: eyr = .false.  ! (Expiration Year)
    logical :: hgt = .false.  ! (Height)
    logical :: hcl = .false.  ! (Hair Color)
    logical :: ecl = .false.  ! (Eye Color)
    logical :: pid = .false.  ! (Passport ID)
    logical :: cid = .false.  ! (Country ID)
end type record

type(record), dimension(:),allocatable :: records
type(record) :: tmp

open(newunit=iunit, file='input.txt', iostat=istat)
if (istat /= 0) error stop ' error reading file'
n_lines = number_of_lines_in_file(iunit)

record_num = 1
records = [tmp]
do j = 1, n_lines

    call read_line_from_file(iunit,chunk_size,line,status_ok)
    if (line == '') then
        record_num = record_num + 1
        records = [records, tmp]
    else
        call split(line,' ',chunk_size,key_vals)
        do i = 1, size(key_vals)
            call split(key_vals(i)%str,':',chunk_size,key_val)
            !write(*,*) key_val(1)%str//' : '//key_val(2)%str
            select case (key_val(1)%str)
            case('byr'); records(record_num)%byr = .true.
            case('iyr'); records(record_num)%iyr = .true.
            case('eyr'); records(record_num)%eyr = .true.
            case('hgt'); records(record_num)%hgt = .true.
            case('hcl'); records(record_num)%hcl = .true.
            case('ecl'); records(record_num)%ecl = .true.
            case('pid'); records(record_num)%pid = .true.
            case('cid'); records(record_num)%cid = .true.
            case default
                error stop 'invalid key'
            end select
        end do
    end if

end do

write(*,*) 'number of records: ', record_num

n_valid = 0
do i = 1, record_num
    if (all([   records(i)%byr, &
                records(i)%iyr, &
                records(i)%eyr, &
                records(i)%hgt, &
                records(i)%hcl, &
                records(i)%ecl, &
                records(i)%pid ])) then
        n_valid = n_valid + 1
    end if
end do
write(*,*) 'n_valid: ', n_valid

end program problem_4