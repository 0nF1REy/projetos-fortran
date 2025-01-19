program operadores_logico
    implicit none
    character :: j1, j2, j3
    logical :: estado, estado_j1, estado_j2, estado_j3

    j1 = 'f'
    j2 = 'f'
    j3 = 'f'

    write (*,*) "Janela 01 aberta? ", j1 == 'a', new_line('a')

    estado = j1 == 'a' .or. j2 == 'a' .or. j3 == 'a'
    write (*,*)"Alguma janela aberta? ", estado

    write(*,*) new_line('a'), " Alarme desligado? ", .not. estado

    estado = j1 == 'a' .and. j2 == 'a' .and. j3 == 'a'
    write(*,*) new_line('a'), " Todas as janelas abertas? ", estado

    estado_j1 = (j1 == 'a')
    estado_j2 = (j2 == 'a')
    estado_j3 = (j3 == 'a')

    estado = (estado_j1 .eqv. estado_j2) .and. (estado_j2 .eqv. estado_j3)
    write(*,'(/, A, 1X, L)') " Todas as janelas estão abertas ou fechadas ao mesmo tempo?", estado

end program operadores_logico