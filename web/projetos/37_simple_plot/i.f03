program simple_plot
    implicit none

    real, parameter :: pi = 4.0 * atan(1.0)
    integer, parameter :: n = 100
    real, dimension(n) :: x, y 
    real, parameter :: a = 0.0, b = 2.0 * pi
    real :: increment
    integer :: i

    increment = (b - a) / real(n - 1)  

    x = [ ( a + real(i-1) * increment, i=1, n ) ]

    y = sin(x)

    open(unit=1, file='data.dat')
    write(1,'(2F10.6)') (x(i), y(i), i=1,n)  
    close(unit=1)

end program simple_plot