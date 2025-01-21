program saida_dados_write
    implicit none
    integer :: polegada, valores
    real :: centimetro
    real, parameter :: cent_polegada = 2.54
    real(kind=16) :: avogadro = 602214000000000000000000.0
    character(8) :: av = 'Avogadro'

    write (*,*) 'Comando WRITE, descritores de formato e comando FORMAT', new_line('A')
    write(*,'(A)') 'Quanto vale uma polegada?'
    write(*,'(A, 1X, F4.2, 1X, A, /)') 'Uma polegada vale', cent_polegada, 'centímetros'
    write(*, '(A)') 'Conversão de polegadas em centímetros'
    write(*, '(A)') 'Digite quantos valores deseja converter:'
    read(*,*) valores
    do polegada = 1, valores
    centimetro = polegada * cent_polegada
    write(*,110) polegada, centimetro
    end do
    110 format(I3, ' polegada = ', F6.2, ' centímetros.')

    write(*, 120) av
    120 format(10X, A)
    print 130, avogadro
    130 format('Número de Avogrado: ', ES12.6)

end program saida_dados_write