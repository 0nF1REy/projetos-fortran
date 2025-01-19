PROGRAM positivo_negativo
    IMPLICIT NONE
    REAL :: numero

    PRINT *, 'Digite um número:'
    READ *, numero

    IF (numero > 0) THEN
        PRINT *, 'O número é positivo.'
    ELSE IF (numero < 0) THEN
        PRINT *, 'O número é negativo.'
    ELSE
        PRINT *, 'O número é zero.'
    END IF
END PROGRAM positivo_negativo