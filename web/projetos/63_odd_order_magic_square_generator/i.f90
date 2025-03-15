PROGRAM odd_order_magic_square_generator
    IMPLICIT NONE

    ! =======================
    !  Declaração de variáveis
    ! =======================
    REAL :: A(100, 100)
    INTEGER :: n, k, d, first, row, col, rowold, colold, last, sum, i, j

    WRITE(*,*) ""
    WRITE(*,*) "====================="
    WRITE(*,*) 'Digite um número ímpar para a dimensão do quadrado mágico'
    READ(*,*) n
    WRITE(*,*) 'Digite o valor mínimo para o elemento da tabela'
    READ(*,*) first

    d = first - 1
    A = d  ! Inicializa a matriz com o valor d
    k = n * n
    row = 1
    col = (n + 1) / 2
    last = first
    A(row, col) = last

    ! Algoritmo para preencher o quadrado mágico
    DO i = 2, k
        colold = col
        rowold = row
        row = row - 1
        col = col + 1
        
        IF (row < 1) THEN
            row = n
        END IF
        
        IF (col > n) THEN
            col = 1
        END IF
        
        IF (A(row, col) == d) THEN
            A(row, col) = first + i - 1
        ELSE
            row = rowold + 1
            IF (row > n) THEN
                row = 1
            END IF
            col = colold
            A(row, col) = first + i - 1
        END IF
    ENDDO

    ! Exibição do quadrado mágico em tabela
    WRITE(*,*) "Quadrado Mágico:"
    WRITE(*,*) "====================="
    DO i = 1, n
        WRITE(*,*) (A(i,j), j = 1, n) 
    ENDDO

    WRITE(*,*) "====================="
    WRITE(*,*) 'Verificação do quadrado mágico'
    WRITE(*,*) "====================="
    
    ! Verificação das somas das linhas, colunas e diagonais
    sum = 0
    DO i = 1, n
        sum = sum + A(1, i)
    ENDDO
    WRITE(*,*) "Soma da primeira linha = ", sum

    ! Verificação das colunas
    DO i = 1, n
        sum = 0
        DO j = 1, n
            sum = sum + A(j, i)
        END DO
        WRITE(*,*) "Soma da coluna ", i, " = ", sum
    ENDDO

    ! Verificação das diagonais
    sum = 0
    DO i = 1, n
        sum = sum + A(i, i)
    ENDDO
    WRITE(*,*) "Soma da diagonal principal = ", sum

    sum = 0
    DO i = 1, n
        sum = sum + A(i, n - i + 1)
    ENDDO
    WRITE(*,*) "Soma da diagonal secundária = ", sum

    WRITE(*,*) "---------------------"
    WRITE(*,*) "Fim do Quadrado Mágico"
    WRITE(*,*) "====================="
    WRITE(*,*) ""

END PROGRAM odd_order_magic_square_generator