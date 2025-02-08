PROGRAM media_tres_numeros
    IMPLICIT NONE

    REAL(KIND=8) :: valores(3), med
    INTEGER :: i, ios
    LOGICAL :: valid_input

    PRINT *, 'Entre com 3 valores...'

    valid_input = .TRUE.  

    DO i = 1, 3
        PRINT *, 'Digite o valor ', i, ':'
        
        DO 
            READ(*, *, IOSTAT=ios) valores(i)

            IF (ios /= 0) THEN
                PRINT *, 'Erro ao ler o valor ', i, '. Por favor, entre com um número válido.'
                PRINT *, 'Digite o valor ', i, ' novamente:'
                ios = 0  
            ELSE
                EXIT 
            END IF
        END DO
    END DO

    IF (valid_input) THEN  
        med = SUM(valores) / 3.0_8

        PRINT *, 'Resultado: ', med
    ELSE
        PRINT *, 'Erro: Impossível calcular a média devido a entradas inválidas.'
    END IF

END PROGRAM media_tres_numeros