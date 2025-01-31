PROGRAM tabuada
  IMPLICIT NONE

  INTEGER :: start_table, end_table, table_limit
  INTEGER :: table, multiplier, product
  INTEGER :: io_status
  LOGICAL :: input_valid

  input_valid = .FALSE.
  DO WHILE (.NOT. input_valid)
    WRITE(*, '("Digite o início da tabuada (inteiro):")')
    READ(*, *, IOSTAT=io_status) start_table
    IF (io_status == 0) THEN
      input_valid = .TRUE.
    ELSE
      WRITE(*, '("Erro: Entrada inválida. Digite um número inteiro.")')
    END IF
  END DO

  input_valid = .FALSE.
  DO WHILE (.NOT. input_valid)
    WRITE(*, '("Digite o fim da tabuada (inteiro):")')
    READ(*, *, IOSTAT=io_status) end_table
    IF (io_status == 0) THEN
       input_valid = .TRUE.
    ELSE
       WRITE(*, '("Erro: Entrada inválida. Digite um número inteiro.")')
    END IF
  END DO

  IF (start_table > end_table) THEN
    WRITE(*, '("Erro: O início da tabuada deve ser menor ou igual ao fim.")')
    STOP
  END IF

  input_valid = .FALSE.
   DO WHILE (.NOT. input_valid)
     WRITE(*, '("Digite o limite do multiplicador (inteiro):")')
     READ(*, *, IOSTAT=io_status) table_limit
    IF (io_status == 0) THEN
       input_valid = .TRUE.
     ELSE
      WRITE(*, '("Erro: Entrada inválida. Digite um número inteiro.")')
     END IF
  END DO

  IF (table_limit <= 0) THEN
    WRITE(*, '("Erro: O limite do multiplicador deve ser um valor positivo.")')
    STOP
  END IF

  DO table = start_table, end_table
    WRITE(*, '(/,A,I0,/,A)') '------ Tabuada do ', table, '------------'
    DO multiplier = 1, table_limit
      product = table * multiplier
      WRITE(*, '(2X,I0," x ",I0," = ",I0)') table, multiplier, product
    END DO
  END DO

  WRITE(*, '(/,A)') "---------------- Fim ------------------"

END PROGRAM tabuada