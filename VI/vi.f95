program parametros_kind
    use iso_fortran_env
    implicit none
    integer, parameter :: sgl = 4
    integer, parameter :: dbl = 8
    integer, parameter :: qbl = 16

    real(kind=4) :: valor1
    real(kind=dbl) :: valor2
    real(kind=qbl) :: valor3
    real(8) :: valor4
    integer(kind=8) :: valor5

    real, parameter :: const = 66._sgl

    write(*,*) 'KIND da variável 1: ', KIND(valor1)
    write(*,*) 'Precisão decimal da variável 1: ', precision(valor1)
    write(*,*) 'Faixa do exponente decimal da variável 1: ', range(valor1)
    write(*,*) 'KIND da variável 2: ', KIND(valor2)
    write(*,*) 'Precisão decimal da variável 2: ', precision(valor2)
    write(*,*) 'Faixa do exponente decimal da variável 2: ', range(valor2)
    write(*,*) 'KIND da variável 3: ', KIND(valor3)
    write(*,*) 'Precisão decimal da variável 3: ', precision(valor3)
    write(*,*) 'Faixa do exponente decimal da variável 3: ', range(valor3)
    write(*,*) 'KIND da variável 4: ', KIND(valor4)
    write(*,*) 'KIND da variável 5: ', KIND(valor5)

    write(*,*) 'KIND do literal 0.0: ', KIND(0.0)
    write(*,*) 'KIND do literal 0.0D0: ', KIND(0.0D0)
    write(*,*) 'Valor da constante: ', const, 'KIND: ', kind(const)

    write(*,*) 'KINDs disponíveis para tipo REAL: ', real_kinds
    write(*,*) 'KINDs disponíveis para tipo INTEGER: ', integer_kinds
    write(*,*) 'KINDs disponíveis para tipo LOGICAL: ', logical_kinds
    write(*,*) 'KINDs disponíveis para tipo CHARACTER: ', character_kinds
end program parametros_kind
