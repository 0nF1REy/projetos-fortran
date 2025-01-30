program guitar_scale_fret_calculator
  implicit none

  ! Constante para cálculo do espaçamento dos trastes
  real, parameter :: FCONST = 2.0**(1.0/12.0)  ! Constante nomeada em caixa alta para clareza

  ! Variáveis de entrada
  real :: scale_length  ! Comprimento da escala
  integer :: total_frets ! Número total de trastes
  integer :: output_option ! Opção de saída selecionada

  ! Variáveis auxiliares
  integer :: fret        ! Contador de trastes
  integer :: io_status   ! Status de operações de I/O
  logical :: valid_input ! Flag para entrada válida

  ! Formatos de saída (mais descritivos e reutilizáveis)
  character(len=*), parameter :: FMT_FRET_POSITION = "(I3, 5X, F8.2)" ! Modificado para F8.2
  character(len=*), parameter :: FMT_HEADER       = "(A4, 5X, A17)"

  ! Mensagem de boas-vindas
  print *, "Calculadora de Posição de Trastes para Guitarra"
  print *, "---------------------------------------------"

  ! -------------------------------------------------------------------
  ! Entrada e validação do comprimento da escala
  ! -------------------------------------------------------------------
  valid_input = .false.
  do while (.not. valid_input)
    print *, 'Digite o comprimento da escala em polegadas (ex: 25.5):'
    read *, scale_length
    if (scale_length <= 0.0) then
      print *, 'Erro: O comprimento da escala deve ser positivo. Tente novamente.'
    else
      valid_input = .true.
    end if
  end do

  ! -------------------------------------------------------------------
  ! Entrada e validação do número total de trastes
  ! -------------------------------------------------------------------
  valid_input = .false.
  do while (.not. valid_input)
    print *, 'Digite o número total de casas (trastes) (máximo 24):'
    read *, total_frets
    if (total_frets <= 0) then
      print *, 'Erro: O número de casas deve ser positivo. Tente novamente.'
    else if (total_frets > 24) then
      print *, 'Erro: O número de casas não pode ser maior que 24. Tente novamente.'
    else
      valid_input = .true.
    end if
  end do

  ! -------------------------------------------------------------------
  ! Seleção da opção de saída
  ! -------------------------------------------------------------------
  print *, "Escolha a forma de saída:"
  print *, "  1 - Salvar em arquivo de texto (.dat)"
  print *, "  2 - Salvar em arquivo CSV (.csv)"
  print *, "  3 - Exibir no terminal"
  print *, "Digite a opção desejada (1, 2 ou 3):"
  read *, output_option

  ! -------------------------------------------------------------------
  ! Processamento e saída dos dados
  ! -------------------------------------------------------------------
  select case (output_option)
    case (1) ! Salvar em arquivo de texto (.dat)
      call write_fret_data('frets.dat', scale_length, total_frets, .false.)

    case (2) ! Salvar em CSV (.csv)
      call write_fret_data('frets.csv', scale_length, total_frets, .true.)

    case (3) ! Exibir no terminal
      print *, 'Resultados para os trastes (em polegadas):'
      print *, FMT_HEADER, "Fret", "Position (inches)"
        call output_fret_positions(scale_length, total_frets)

    case default
      print *, 'Opção inválida! Por favor, digite 1, 2 ou 3.'
      stop
  end select

contains

  ! Subrotina para cálculo e saída de posições de trastes
    subroutine output_fret_positions(scale_length, total_frets)
    real, intent(in) :: scale_length
    integer, intent(in) :: total_frets
    integer :: fret
    do fret = 1, total_frets
       write(*, fmt=FMT_FRET_POSITION) fret, scale_length / (FCONST ** fret)
    end do
  end subroutine output_fret_positions

  ! Subrotina para escrever dados de trastes em arquivo
  subroutine write_fret_data(filename, scale_length, total_frets, header)
    character(len=*), intent(in) :: filename
    real, intent(in) :: scale_length
    integer, intent(in) :: total_frets
    logical, intent(in) :: header
    integer :: fret, io_status
    open(unit=23, file=filename, status='replace', iostat=io_status)
    if (io_status /= 0) then
      print *, 'Erro ao abrir o arquivo ', filename, '.'
      stop
    end if

    if (header) then
      write(unit=23, fmt=FMT_HEADER) "Fret", "Position (inches)"
    end if
       do fret = 1, total_frets
          write(unit=23, fmt=FMT_FRET_POSITION) fret, scale_length / (FCONST ** fret)
       end do

    close(unit=23)
    print *, 'Cálculos concluídos. Resultados gravados em ', filename, '.'

  end subroutine write_fret_data

end program guitar_scale_fret_calculator