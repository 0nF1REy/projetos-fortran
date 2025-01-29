program linspace_squares 
    use my_lib
    implicit none
    
    integer, parameter :: len_x = 10
    real, dimension (1:len_x) :: x, y
    integer :: i

    call linspace (x, 1.0, 10.0, len_x)
    y = x**2

    do i = 1,len_x
       write (*, '(f6.1, a, f6.1)') x(i), '  ', y(i)
    end do

end program linspace_squares