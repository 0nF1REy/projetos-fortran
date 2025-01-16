program variaveis
    implicit none
    integer :: idade, quantidade
    real :: peso
    character(len=20) :: nome
    logical :: estado = .true.
    complex :: tensao

    real, parameter :: fator_mult = 4.56

    quantidade = 50
    peso = 52.70
    nome = 'Alan Ryan'
    idade = 20
    tensao = (12.0, 2.0)

    write(*,*) 'Quantidade: ', quantidade
    write(*,*) 'Peso: ', peso
    write(*,*) 'Nome: ', nome
    write(*,*) 'Idade: ', idade
    write(*,*) 'Status: ', estado
    write(*,*) 'Fator: ', fator_mult
    write(*,*) 'Tensão: ', tensao
    write(*,*) 'Maior valor inteiro: ', huge(quantidade)
    write(*,*) 'Menor valor real positivo: ', tiny(peso)
end program variaveis