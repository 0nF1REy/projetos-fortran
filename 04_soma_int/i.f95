program soma_int
    implicit none
    integer :: num_one, num_two, result
    print *, 'Digite o primeiro número:'
    read *, num_one
    print *, 'Digite o segundo número:'
    read *, num_two
    result = num_one + num_two
    print *, 'Resultado da somatória: ', result
end program soma_int
