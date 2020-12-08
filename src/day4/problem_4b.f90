program problem_4b

!! In your batch file, how many passports are valid?

use aoc_utilities
use iso_fortran_env

implicit none

integer,parameter :: chunk_size = 256

integer :: iunit, istat, n_lines, record_num, i, j, n_valid, ival, n, c
character(len=:),allocatable :: line, key, val
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
            key = key_val(1)%str
            val = key_val(2)%str
            n = len(val)
            select case (key)
            case('byr') ! four digits; at least 1920 and at most 2002.
                if (len(val)==4 .and. verify(val,'0123456789')==0) then
                    read(val, '(I4)') ival
                    records(record_num)%byr = ival >= 1920 .and. ival <= 2002
                end if
            case('iyr') ! four digits; at least 2010 and at most 2020.
                if (len(val)==4 .and. verify(val,'0123456789')==0) then
                    read(val, '(I4)') ival
                    records(record_num)%iyr = ival >= 2010 .and. ival <= 2020
                end if
            case('eyr') ! four digits; at least 2020 and at most 2030.
                if (len(val)==4 .and. verify(val,'0123456789')==0) then
                    read(val, '(I4)') ival
                    records(record_num)%eyr = ival >= 2020 .and. ival <= 2030
                end if
            case('hgt') ! a number followed by either cm or in:
                        ! If cm, the number must be at least 150 and at most 193.
                        ! If in, the number must be at least 59 and at most 76.
                if (n>2) then
                    if (verify(val(1:n-2),'0123456789')==0) then
                        read(val(1:n-2),*) ival
                        if (val(n-1:n)=='cm' ) then
                            records(record_num)%hgt = ival>=150 .and. ival<=193
                        elseif (val(n-1:n)=='in') then
                            records(record_num)%hgt = ival>=59 .and. ival<=76
                        end if
                    end if
                end if
            case('hcl') ! a # followed by exactly six characters 0-9 or a-f.
                !records(record_num)%hcl = .true.
                if (n==7) then
                    if (val(1:1)=='#') then
                        do c = 2, n
                            if ((val(c:c)>='0' .and. val(c:c)<='9') .or. &
                                (val(c:c)>='a' .and. val(c:c)<='f')) then
                                !ok
                            else
                                cycle
                            end if
                        end do
                        records(record_num)%hcl = .true. ! all ok
                    end if
                end if
            case('ecl') ! exactly one of: amb blu brn gry grn hzl oth.
                records(record_num)%ecl = any(val==['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'])
            case('pid') ! a nine-digit number, including leading zeroes.
                records(record_num)%pid = n==9 .and. verify(val,'0123456789')==0
            case('cid') ! ignored, missing or not.
                records(record_num)%cid = .true.
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

end program problem_4b