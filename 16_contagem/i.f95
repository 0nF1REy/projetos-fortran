program contagem
    implicit none

    integer :: cont
    character(20) :: frase = 'Você é demais!'

    do cont = 1, 500000
      write(*,'(A)') frase
    end do

    write(*,*)

end program contagem