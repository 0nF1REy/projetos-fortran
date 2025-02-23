PROGRAM matrix_transpose_print
    IMPLICIT NONE

    INTEGER, DIMENSION(4) :: vec
    INTEGER :: mat(3,3)

    vec = (/2,3,4,5/)

    PRINT *, " "
    PRINT *, vec
    PRINT *, " "

    !    | valores de mat |
    !    |                |
    !    |     1 4 7      |      
    !    |     2 5 8      |
    !    |     3 6 9      |

    mat = RESHAPE((/1,2,3,4,5,6,7,8,9/),SHAPE(mat))

    CALL printmat(mat)

    mat = TRANSPOSE(mat)

    CALL printmat(mat)

CONTAINS

    SUBROUTINE printmat(m)
        INTEGER, DIMENSION(:,:), INTENT(IN) :: m
        INTEGER :: i, j

        PRINT *, "Matriz:"
        DO i = 1, SIZE(m, 1)
            PRINT *, (m(i, j), j = 1, SIZE(m, 2))
        END DO
        PRINT *, " "
    END SUBROUTINE printmat

END PROGRAM matrix_transpose_print
