PROGRAM sequencia_fibonacci
  IMPLICIT NONE

  INTEGER :: a, b, next, n, i

  WRITE(*,*) ''
  WRITE(*,*) '===================================================='
  WRITE(*,*) 'Digite o número de termos na sequência de Fibonacci:'
  READ(*,*) n

  IF (n <= 0) THEN
    WRITE(*,*) 'Erro: O número de termos deve ser um inteiro positivo.'
    STOP
  END IF

  a = 0
  b = 1

  WRITE(*,*) '-----------------------'
  WRITE(*,*) '|   Termo   |   Valor   |'
  WRITE(*,*) '-----------------------'

  DO i = 1, n
    IF (i == 1) THEN
      WRITE(*,'(A1,I7,A7,I7,A1)') '|', i, '      |', a, '      |'
    ELSE IF (i == 2) THEN
      WRITE(*,'(A1,I7,A7,I7,A1)') '|', i, '      |', b, '      |'
    ELSE
      next = a + b
      WRITE(*,'(A1,I7,A7,I7,A1)') '|', i, '      |', next, '      |'
      a = b
      b = next
    END IF
  END DO

  WRITE(*,*) '-----------------------'

END PROGRAM sequencia_fibonacci