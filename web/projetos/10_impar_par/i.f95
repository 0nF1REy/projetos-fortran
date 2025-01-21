PROGRAM impar_par
    IMPLICIT NONE
    INTEGER :: numero
    DO
        PRINT *, 'Digite um número (-1 para parar):'
        READ *, numero

        IF (numero == -1) THEN
            EXIT
        END IF

        IF (MOD(numero, 2) == 0) THEN
            PRINT *, 'O número é par.'
        ELSE
            PRINT *, 'O número é ímpar.'
        END IF
    END DO
    PRINT *, 'Programa finalizado.'
END PROGRAM impar_par
