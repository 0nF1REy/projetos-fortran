PROGRAM inicia_declara
    IMPLICIT NONE

    ! Sempre podemos dar valores iniciais às nossas variáveis na declaração

    REAL(KIND=4)        :: pi = 4.*ATAN(1.)
    INTEGER(KIND=4)     :: i    = 10.5, j = 20
    CHARACTER(LEN=10)   :: nome = "Alan Ryan"

    PRINT *, pi
    PRINT *, i, j
    PRINT *, nome

END PROGRAM inicia_declara