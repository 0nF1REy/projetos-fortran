program metodo_bissecao
    implicit none
    real(8) :: a, b, c, fa, fb, fc, tol
    integer :: max_iter, i

    print*, 'Digite o valor de a (extremidade inferior):'
    read*, a
    print*, 'Digite o valor de b (extremidade superior):'
    read*, b
    print*, 'Digite a tolerância desejada:'
    read*, tol
    print*, 'Digite o número máximo de iterações:'
    read*, max_iter

    if (a == b) then
        print*, 'Erro: os valores de a e b não podem ser iguais.'
        stop
    end if
    if (tol <= 0.0d0) then
        print*, 'Erro: a tolerância deve ser maior que zero.'
        stop
    end if
    if (max_iter <= 0) then
        print*, 'Erro: o número máximo de iterações deve ser maior que zero.'
        stop
    end if

    ! Calcula o valor da função nos extremos do intervalo
    fa = func(a)
    fb = func(b)

    ! Verifica se há troca de sinal no intervalo
    if (fa * fb > 0.0d0) then
        print*, 'A função não tem sinais opostos no intervalo!'
        stop
    end if

    do i = 1, max_iter
        c = (a + b) / 2.0d0  ! Calcula o ponto médio do intervalo
        fc = func(c)        ! Calcula o valor da função no ponto médio

        if (abs(fc) < tol) then
            print*, 'Raiz encontrada: ', c
            print*, 'Número de iterações: ', i
            stop
        end if

        ! Atualiza o intervalo com base no sinal da função no ponto médio
        if (fa * fc < 0.0d0) then
            b = c
            fb = fc
        else
            a = c
            fa = fc
        end if
    end do

    print*, 'Número máximo de iterações atingido sem encontrar a raiz.'
    
contains

    ! Função cuja raiz será calculada (f(x) = x^2 - 2)
    real(8) function func(x)
        real(8), intent(in) :: x
        func = x**2 - 2.0d0  
    end function

end program