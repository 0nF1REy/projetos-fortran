program unicode_writer
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none

    integer, parameter :: ucs2 = selected_char_kind('ISO_10646')
    character(kind=ucs2, len=:), allocatable :: str
    integer :: iostat, i, len_trim
    character(len=256) :: error_message  

    if (ucs2 == 0) then
        write (error_unit, *) "Error: UCS-2 character kind is not supported by this compiler."
        stop 1
    end if

    allocate(character(kind=ucs2, len=25) :: str, stat=iostat)
    if (iostat /= 0) then
        write (error_unit, *) "Error allocating memory for string. IOSTAT =", iostat
        stop 4
    end if

    str = ucs2_"Unicode character: " // char(128640, kind=ucs2)

    open (output_unit, encoding='utf-8', iostat=iostat, iomsg=error_message)
    if (iostat /= 0) then
        write (error_unit, *) "Error opening output unit with UTF-8 encoding. IOSTAT =", iostat
        write (error_unit, *) "Error message:", trim(error_message) 
        stop 2
    end if

    write (output_unit, '(a)', iostat=iostat, iomsg=error_message) str
    if (iostat /= 0) then
        write (error_unit, *) "Error writing to output unit. IOSTAT =", iostat
        write (error_unit, *) "Error message:", trim(error_message)  
        close (output_unit)
        stop 3
    end if

    close(output_unit, iostat=iostat, iomsg=error_message)
    if (iostat /= 0) then
        write (error_unit, *) "Error closing output unit. IOSTAT =", iostat
        write (error_unit, *) "Error message:", trim(error_message) 
        stop 5  
    end if

    deallocate(str, stat=iostat)
    if (iostat /= 0) then
       write (error_unit, *) "Error deallocating memory for string. IOSTAT =", iostat
       stop 6
    end if

end program unicode_writer