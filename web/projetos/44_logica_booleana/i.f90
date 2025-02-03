program logica_booleana
    implicit none

    logical :: verdadeiro, falso, compara_igual, compara_menor, resultado_and, resultado_or, resultado_not
    integer :: a, b, c

    verdadeiro = .true.
    falso = .false.

    print *, "-----------------------------------"
    print *, "Valor de 'verdadeiro': ", verdadeiro
    print *, "Valor de 'falso': ", falso

    a = 10
    b = 11
    c = 10

    compara_igual = (a == b)
    compara_menor = (a < b)

    resultado_and = (a == c) .and. (a < b)
    resultado_or  = (a == b) .or. (a < c)
    resultado_not = .not. (a == b)

    print *, "-----------------------------------"
    print *, "Comparação (a == b): ", compara_igual
    print *, "Comparação (a < b): ", compara_menor
    print *, "-----------------------------------"
    print *, "Resultado do (AND): ", resultado_and
    print *, "Resultado do (OR):  ", resultado_or
    print *, "Resultado do (NOT): ", resultado_not
    print *, "-----------------------------------"

end program logica_booleana
