program fatorial
    implicit none

    integer :: n, i, fat = 1

    write(*,*) 'Programa que calcula o fatorial de um número'
    write(*,*)
    write(*,*) 'Digite um número para calcular seu fatorial:'
    read(*,*) n

    do i = 1, n
      fat = fat * i
    end do 
    write(*, '(A, I0)') 'O fatorial é ', fat

end program fatorial