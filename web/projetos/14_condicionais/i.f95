program condicionais
    implicit none

    integer :: num = 0
    character(35) :: msg = 'Digite um número:'
    write(*, '(A)') msg
    read(*,*) num

    if(num > 10) then 
    write(*, '(A)') 'Este é um número maior que 10.'
    else if(num < 10) then 
    write(*, '(A)') 'Este é um número menor do que 10.'
    else
    write(*, '(A)') 'Este é um número igual a 10.'
    end if

end program condicionais