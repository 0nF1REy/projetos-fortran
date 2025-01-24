program area_circulo
    implicit none

    real :: raio1, raio2, area1, area2, pi

    pi = 4.0*atan(1.0)

    open(unit = 1, file = "raios.dat") ! unit = 1 --->  entrada 
    read(1,*); read(1,*); read(1,*);
    read(1,*) raio1
    read(1,*) raio2
    close(unit=1)

    print *, pi, raio1, raio2

    area1 = pi*(raio1**2)
    area2 = pi*(raio2**2)

    print *, "Area 1 =", area1
    print *, "Area 2 =", area2

    open(unit = 2, file = "areas.dat") ! unit = 2 --->  saída 
    write(2,*) "Area do circulo 1 =", area1
    write(2,*) "Area do circulo 2 =", area2
    close(unit = 2)

end program area_circulo