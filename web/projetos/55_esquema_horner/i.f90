program esquema_horner
  implicit none

  integer, parameter :: MAX_GRAU = 100
  integer :: grau
  real, allocatable :: coeficientes(:)
  real :: valor_x, valor_polinomio
  integer :: i, io_status
  character(len=3) :: max_grau_char

  write(max_grau_char, '(i3)') MAX_GRAU

  print *, 'Insira o grau do polinômio (0 a ' // trim(max_grau_char) // '):'
  read (*, *, iostat=io_status) grau

  if (io_status /= 0) then
    print *, 'Erro ao ler o grau. Por favor, insira um inteiro.'
    stop
  end if
  if (grau < 0 .or. grau > MAX_GRAU) then
    print *, 'Grau inválido. O grau deve estar entre 0 e ', MAX_GRAU
    stop
  end if

  allocate(coeficientes(0:grau), stat=io_status)
  if (io_status /= 0) then
    print *, 'Erro ao alocar memória para os coeficientes.'
    stop
  end if

  print *, 'Insira os coeficientes, do termo constante ao grau mais alto:'
  do i = 0, grau
    print *, 'Coeficiente para x^', i, ':'
    read (*, *, iostat=io_status) coeficientes(i)

    if (io_status /= 0) then
      print *, 'Erro ao ler o coeficiente ', i, '. Por favor, insira um número.'
      deallocate(coeficientes)
      stop
    end if
  end do

  print *, 'Coeficientes inseridos:'
  print *, coeficientes

  print *, 'Insira o valor de x:'
  read (*, *, iostat=io_status) valor_x

  if (io_status /= 0) then
    print *, 'Erro ao ler o valor de x. Por favor, insira um número.'
    deallocate(coeficientes)
    stop
  end if

  valor_polinomio = coeficientes(grau)
  do i = grau - 1, 0, -1
    valor_polinomio = valor_polinomio * valor_x + coeficientes(i)
  end do

  print *, 'O valor do polinômio em x = ', valor_x, ' é: ', valor_polinomio

  deallocate(coeficientes)

end program esquema_horner