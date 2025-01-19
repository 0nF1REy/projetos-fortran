program operadores_aritmeticos
    implicit none
    integer :: num1, num2, resultado

    write(*,*) "Digite um número:"
    read(*,*) num1
    write(*,*) "Digite outro número:"
    read(*,*) num2

    resultado = num1 + num2
    write(*,*) "Soma: ", resultado
    resultado = num1 - num2
    write(*,*) "Subtração: ", resultado
    resultado = num1 * num2
    write(*,*) "Multiplicação: ", resultado
    resultado = num1 / num2
    write(*,*) "Divisão: ", num1 / real(num2)
    resultado = num1 ** num2
    write(*,*) "Exponenciação: ", resultado

    write(*,*) "Resultado da expressão: ", (num1 + num2) * num1 - num2 ** num1 / 2.0

    write(*,*) "Resultado da divisão inteira: ", mod(num1, num2)

end program operadores_aritmeticos