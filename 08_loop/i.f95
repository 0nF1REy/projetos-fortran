PROGRAM loop
    IMPLICIT NONE
    INTEGER :: i
    REAL :: start_time, end_time, elapsed_time

    CALL CPU_TIME(start_time)

    DO i = 1, 10000000
        PRINT *, 'Número: ', i
    END DO

    CALL CPU_TIME(end_time)

    elapsed_time = end_time - start_time

    PRINT *, 'Tempo total de execução: ', elapsed_time, ' segundos'
END PROGRAM loop
