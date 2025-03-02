PROGRAM sphere_vol_diff
    IMPLICIT NONE

    ! Variáveis
    DOUBLE PRECISION :: rad1, rad2, vol1, vol2
    CHARACTER(LEN=1) :: resposta
    INTEGER :: iostat
    DOUBLE PRECISION, PARAMETER :: pi = 4.0*ATAN(1.0)

DO
    PRINT *, 'Por favor, digite o raio da primeira esfera (rad1) e o raio da segunda esfera (rad2):'
    READ(UNIT=*, FMT=*, IOSTAT=iostat) rad1, rad2

    IF (iostat /= 0) THEN
        PRINT *, "Entrada inválida. Por favor, digite números."
        CYCLE  
    END IF

    IF (rad1 < 0 .OR. rad2 < 0) THEN
        PRINT *, "Os raios devem ser não negativos."
        CYCLE  
    END IF

    ! Calcula os volumes vol1 e vol2
    CALL volume(rad1, vol1)
    CALL volume(rad2, vol2)

    WRITE(*,10) 'A diferença entre os volumes é: ', abs(vol1-vol2)

10  FORMAT(a,f10.3)

    PRINT *, 'Executar novamente? Digite S para sim, caso contrário, digite qualquer outra tecla:'
    READ *, resposta
    IF(resposta /= 'S' .and. resposta /= 's') STOP 

END DO

CONTAINS

    SUBROUTINE volume(rad, vol)
        IMPLICIT NONE
        DOUBLE PRECISION :: rad, vol

        ! Calcula o volume
        vol = (4.0/3.0)*pi*rad**3

    END SUBROUTINE volume

END PROGRAM sphere_vol_diff