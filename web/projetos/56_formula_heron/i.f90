PROGRAM formula_heron
    IMPLICIT NONE

    REAL :: a, b, c, s, area

    WRITE(*,*) 'Digite os comprimentos dos três lados do triângulo (a, b, c):'
    READ(*,*) a, b, c

    IF (a <= 0 .OR. b <= 0 .OR. c <= 0) THEN
        WRITE(*,*) 'Erro: Os lados devem ser positivos.'
        STOP
    END IF

    IF (a + b <= c .OR. a + c <= b .OR. b + c <= a) THEN
        WRITE(*,*) 'Erro: Os lados fornecidos não formam um triângulo válido (desigualdade triangular violada).'
        STOP
    END IF

    s = (a + b + c) / 2.0

    area = SQRT(s * (s - a) * (s - b) * (s - c))

    WRITE(*,*) 'Semiperímetro (s) = ', s
    WRITE(*,*) 'Área do triângulo = ', area

END PROGRAM formula_heron