program multiplica_valores
    implicit none
    integer :: i 
    integer :: valor
    i = 40 
    valor = 10
    valor = valor * 5
    50 valor = valor * 10
    write(*,*) 'Valor de valor: ', valor
end program multiplica_valores
