program soma_100

    implicit none  

    integer :: isoma, i, ind, is

    isoma = 0
    do i=1, 100
      isoma = isoma+i
        if(isoma <= 100) then  
            is = isoma
            else
            ind = i - 1
            goto 200
        end if
    enddo

200 continue

    write(*,*) 'O número de valores é: ',ind
    write(*,*) 'A soma dos valores é: ',is

end program soma_100