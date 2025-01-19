program par
    integer :: num = 0

    do while(num >= 0 )
        write(*,*) 'Digite um número positivo, ou um negativo para parar:'
        read(*,*) num

        if (mod(num,2) == 0 ) then
            write(*,*) 'O número é par.'
        else 
            write(*,*) 'O número é impar.'
        end if
    end do

end program par