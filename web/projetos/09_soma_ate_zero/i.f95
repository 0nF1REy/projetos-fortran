PROGRAM soma_ate_zero
    IMPLICIT NONE
    REAL :: numero, soma

    soma = 0.0
    DO WHILE (numero /= 0.0)
        PRINT *, 'Digite um número (0 para parar):'
        READ *, numero
        soma = soma + numero
    END DO

    PRINT *, 'A soma dos números é: ', soma
END PROGRAM soma_ate_zero
