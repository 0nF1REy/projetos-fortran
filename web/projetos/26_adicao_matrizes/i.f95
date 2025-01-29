program adicao_matrizes
    implicit none

    real, allocatable :: A(:,:), B(:,:), C(:,:)
    integer :: m, n, p, q, i, j

    write(*,*) 'Digite a ordem da matriz A (m, n):'
    read(*,*) m, n
    write(*,*) 'Digite a ordem da matriz B (p, q):'
    read(*,*) p, q

    if (m /= p .or. n /= q) then
        write(*,*) 'As dimensões das matrizes não são compatíveis para soma.'
        stop
    end if

    allocate(A(m, n), B(p, q), C(m, n))

    write(*,*) 'Digite os elementos da matriz A:'
    do i = 1, m
        read(*,*) (A(i, j), j = 1, n)
    end do

    write(*,*) 'A matriz A é:'
    call exibir_matriz(A, m, n)

    write(*,*) 'Digite os elementos da matriz B:'
    do i = 1, p
        read(*,*) (B(i, j), j = 1, q)
    end do

    write(*,*) 'A matriz B é:'
    call exibir_matriz(B, p, q)

    do i = 1, m
        do j = 1, n
            C(i, j) = A(i, j) + B(i, j)
        end do
    end do

    write(*,*) 'A matriz soma C é:'
    call exibir_matriz(C, m, n)

    deallocate(A, B, C)

contains

    subroutine exibir_matriz(M, rows, cols)
        real, intent(in) :: M(:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j
        do i = 1, rows
            write(*,*) (M(i, j), j = 1, cols)
        end do
    end subroutine exibir_matriz

end program adicao_matrizes
