PROGRAM subr
    IMPLICIT NONE
    REAL :: x, y, result

    READ*, x
    READ*, y

    CALL soma(x, y, result)

    PRINT*, "Valores:", x, y, "Resultado:", result

END PROGRAM subr

SUBROUTINE soma(a, b, c)
    IMPLICIT NONE
    
    REAL, INTENT(IN)  :: a, b
    REAL, INTENT(OUT) :: c

    c = a + b

END SUBROUTINE soma