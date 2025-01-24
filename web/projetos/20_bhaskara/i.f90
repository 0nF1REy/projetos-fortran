program bhaskara
    implicit none
    integer :: a, b, c
    real :: delta, x1, x2
    real :: parte_real, parte_imaginaria

    a = 4
    b = -3
    c = -1

    delta = b**2 - 4*a*c
    print *, "Delta = ", delta

    if (delta >= 0) then
        x1 = (-b + sqrt(delta)) / (2*a)
        x2 = (-b - sqrt(delta)) / (2*a)
        print *, "x1 = ", x1
        print *, "x2 = ", x2
    else
        parte_real = -b / (2*a)
        parte_imaginaria = sqrt(-delta) / (2*a)
        print *, "Raízes Complexas:"
        print *, "x1 = ", parte_real, " + ", parte_imaginaria, "i"
        print *, "x2 = ", parte_real, " - ", parte_imaginaria, "i"
    end if

end program bhaskara