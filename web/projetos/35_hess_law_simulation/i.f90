program hess_law_simulation
  implicit none

  ! --- Type Definitions ---
  type reaction_data
    real :: enthalpy         ! Enthalpy change of the reaction
    real :: temperature      ! Temperature at which the reaction occurs
    real :: delta_G          ! Gibbs Free Energy change (ΔG)
    real :: entropy          ! Entropy change of the reaction (ΔS)
    integer :: multiplier    ! Multiplier for the reaction
    logical :: reversed      ! True if the reaction is reversed
    logical :: is_equilibrium ! True if the reaction is in equilibrium
    logical :: is_redox      ! True if the reaction is a redox reaction
    integer :: phase            ! Phase of the reactants/products (1 for gas, 2 for liquid, 3 for solid)
  end type reaction_data

  ! --- Variable Declarations ---
  integer, parameter :: MAX_REACTIONS = 10 ! Maximum number of reactions allowed
  type(reaction_data) :: reactions(MAX_REACTIONS)
  integer :: num_reactions
  real :: total_enthalpy
  real :: total_delta_G
  integer :: i
  character(len=1) :: input_choice
  character(len=32) :: max_reactions_str
  logical :: is_reversed
  logical :: is_equilibrium
  logical :: is_redox
  real :: temperature_diff, delta_H_temp

  ! --- Main Program ---
  ! Initialize reaction data
  num_reactions = 0
  total_enthalpy = 0.0
  total_delta_G = 0.0

  print *, "Bem-vindo(a) à Calculadora da Lei de Hess!"

  write(max_reactions_str, "(i0)") MAX_REACTIONS
  do
    ! Get the number of reactions from the user
    call read_integer("Digite o número de reações a serem usadas (máx. " // trim(max_reactions_str) // "): ", num_reactions)
    if (num_reactions > 0 .and. num_reactions <= MAX_REACTIONS) exit
    print*, "Número de reações inválido, tente novamente."
  end do

  ! Input the data for each reaction
  do i = 1, num_reactions
    print *, "---- Reação ", i, " ---- (Início da Entrada de Dados)"
    
    call read_real("Digite a variação de entalpia (kJ): ", reactions(i)%enthalpy)
    call read_integer("Digite o multiplicador estequiométrico (1 se nenhum): ", reactions(i)%multiplier)
    
    ! Temperature-dependent enthalpy calculation using Kirchhoff's Law
    print *, "Digite a temperatura da reação (K):"
    call read_real("Temperatura (K): ", reactions(i)%temperature)
    print *, "Digite a variação da entalpia com a temperatura (ΔH(T)):"
    call read_real("ΔH(T) em kJ/K: ", delta_H_temp)
    call read_real("Diferença de temperatura (ΔT) em K: ", temperature_diff)
    reactions(i)%enthalpy = reactions(i)%enthalpy + delta_H_temp * temperature_diff

    call read_real("Digite a variação de entropia (J/K): ", reactions(i)%entropy)

    do
        write(*, "(a)", advance="no") "Reverter a reação? (S/n): "
        read*, input_choice
        if (input_choice == "s" .or. input_choice == "n") exit
        print*, "Entrada inválida, digite 's' ou 'n'."
    end do
    is_reversed = (input_choice == "s")
    reactions(i)%reversed = is_reversed

    do
        write(*, "(a)", advance="no") "A reação está em equilíbrio? (S/n): "
        read*, input_choice
        if (input_choice == "s" .or. input_choice == "n") exit
        print*, "Entrada inválida, digite 's' ou 'n'."
    end do
    is_equilibrium = (input_choice == "s")
    reactions(i)%is_equilibrium = is_equilibrium
    
    do
        write(*, "(a)", advance="no") "É uma reação redox? (S/n): "
        read*, input_choice
        if (input_choice == "s" .or. input_choice == "n") exit
        print*, "Entrada inválida, digite 's' ou 'n'."
    end do
    is_redox = (input_choice == "s")
    reactions(i)%is_redox = is_redox

    call read_integer("Digite o fase da reação (1=gas, 2=liquido, 3=solido): ", reactions(i)%phase)
    
    ! If the reaction is reversed, negate the enthalpy and entropy changes for easier calculation
    if (reactions(i)%reversed) then
        reactions(i)%enthalpy = -reactions(i)%enthalpy
        ! reactions(i)%entropy = -reactions(i)%entropy ! Não precisa inverter a entropia aqui, faremos isso no cálculo do deltaG
    end if
    print *, "---- Reação ", i, " ---- (Fim da Entrada de Dados)"
  end do
  
  ! Calculate total enthalpy change and total Gibbs Free Energy change
  do i = 1, num_reactions
      total_enthalpy = total_enthalpy + reactions(i)%multiplier * reactions(i)%enthalpy
       ! Calculate Gibbs Free Energy Change (ΔG = ΔH - TΔS)
       if (reactions(i)%reversed) then
         reactions(i)%delta_G = reactions(i)%enthalpy - reactions(i)%temperature * (-reactions(i)%entropy)/1000.0
       else
         reactions(i)%delta_G = reactions(i)%enthalpy - reactions(i)%temperature * reactions(i)%entropy/1000.0
       end if
      total_delta_G = total_delta_G + reactions(i)%multiplier * reactions(i)%delta_G
  end do

  ! Output results
  print *, "------------------------------------"
  print *, " Resultados do Cálculo da Lei de Hess "
  print *, "------------------------------------"
  print *, "Detalhes das Reações Individuais: "
  do i = 1, num_reactions
    print *, "Reação ", i, ": "
    print *, "  Entalpia = ", reactions(i)%enthalpy, " kJ"
    print *, "  Entropia = ", reactions(i)%entropy, " J/K"
    print *, "  Multiplicador = ", reactions(i)%multiplier
    print *, "  Revertida = ", reactions(i)%reversed
    print *, "  Equilíbrio = ", reactions(i)%is_equilibrium
    print *, "  Redox = ", reactions(i)%is_redox
    print *, "  Fase = ", reactions(i)%phase
    print *, "  Energia Livre de Gibbs (ΔG) = ", reactions(i)%delta_G, " kJ"
  end do
  print *, "------------------------------------"
  print *, "Variação Total da Entalpia (Delta H): ", total_enthalpy, " kJ"
  print *, "Variação Total da Energia Livre de Gibbs (Delta G): ", total_delta_G, " kJ"
  print *, "------------------------------------"

contains

  ! Function to read real numbers
  subroutine read_real(prompt, result)
    implicit none
    character(len=*), intent(in) :: prompt
    real, intent(out) :: result
    character(len=256) :: input_str
    integer :: status

    do
        write(*, "(a)", advance="no") prompt
        read(*, "(a)", iostat=status) input_str
        if (status == 0) then
          read(input_str, *, iostat=status) result
          if (status == 0) then
            exit
          else
            print*, "Formato de número real inválido. Tente novamente."
          end if
        else
          print*, "Erro ao ler a entrada. Tente novamente."
        end if
    end do
  end subroutine read_real

  ! Function to read integer numbers
  subroutine read_integer(prompt, result)
    implicit none
    character(len=*), intent(in) :: prompt
    integer, intent(out) :: result
    character(len=256) :: input_str
    integer :: status

    do
        write(*, "(a)", advance="no") prompt
        read(*, "(a)", iostat=status) input_str
        if (status == 0) then
          read(input_str, *, iostat=status) result
          if (status == 0) then
            exit
          else
            print*, "Formato de número inteiro inválido. Tente novamente."
          end if
        else
          print*, "Erro ao ler a entrada. Tente novamente."
        end if
    end do
  end subroutine read_integer

end program hess_law_simulation