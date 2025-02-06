PROGRAM implicit_none
    !IMPLICIT NONE

    pi = 4.*ATAN(1.0)
    aviao = 5.d0
    inhame = 2.5
    joelho = 3
    mao = 2

    PRINT*, "pi =", pi, ", aviao = ", aviao
    PRINT*, "inhame = ", inhame, ", joelho = ", joelho, ", mao = ", mao
    PRINT*, mao/joelho

    ! ========================================================
    ! Sem IMPLICIT NONE, as variáveis cujos nomes comecem por:
    ! ========================================================
    !             i, j, k, l, m, n : integer
    ! ========================================================
    !              (a-h) e (o-z)  : real
    ! ========================================================

END PROGRAM implicit_none