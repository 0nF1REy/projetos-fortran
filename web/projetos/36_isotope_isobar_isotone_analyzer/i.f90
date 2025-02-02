program isotope_isobar_isotone_analyzer
    implicit none

    integer, parameter :: NUM_ELEMENTOS = 118
    character(len=2), dimension(NUM_ELEMENTOS) :: elementos = &
       (/'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
         'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', &
         'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
         'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', &
         'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
         'Sb', 'I ', 'Te', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', &
         'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
         'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', &
         'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
         'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', &
         'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds', &
         'Rg', 'Cn', 'Nh', 'Fl', 'Mc', 'Lv', 'Ts', 'Og'/)

    integer :: Z, A, i, isotone_Z, ios, contagem_isobaros, contagem_isotones
    integer :: neutrons, neutrons_atuais, isotone_A
    logical :: limite_isobaros_atingido, limite_isotones_atingido
    character(len=2) :: elemento
    logical :: continue_analysis

    continue_analysis = .true.
    
    do while (continue_analysis)
      ! Solicitar entrada
      print *, "Digite o número atômico (Z) do elemento (ou 0 para sair):"
      read(*, *, iostat=ios) Z
      if (ios /= 0) then
        print *, "Entrada inválida para o número atômico. Por favor, digite um número inteiro."
        cycle  ! Continue to the next loop iteration
      end if

      if (Z == 0) then
        continue_analysis = .false.
        cycle
      end if

      print *, "Digite o número de massa (A) do isótopo:"
      read(*, *, iostat=ios) A
      if (ios /= 0) then
          print *, "Entrada inválida para o número de massa. Por favor, digite um número inteiro."
          cycle
      end if

      ! Validar entrada
      if (Z < 1 .or. Z > NUM_ELEMENTOS) then
          print *, "Número atômico inválido! Por favor, digite um número atômico válido entre 1 e ", NUM_ELEMENTOS, "."
          cycle
      end if

      if (A <= Z) then
           print *, "Número de massa inválido! O número de massa deve ser maior que o número atômico."
           cycle
       end if

       ! Process the input and perform calculations
        call analyze_nuclide(Z, A, elementos)

        print *, " "
        print *, "Deseja realizar outra análise? (s/n)"
        read (*, "(a)") elemento
        if (elemento /= "s ") then
            continue_analysis = .false.
        end if
    end do
    print *, "Programa finalizado."
contains

    subroutine analyze_nuclide(Z, A, elementos)
       implicit none
       integer, parameter :: NUM_ELEMENTOS = 118
       integer :: Z, A, i, isotone_Z, ios, contagem_isobaros, contagem_isotones
       integer :: neutrons, neutrons_atuais, isotone_A
       logical :: limite_isobaros_atingido, limite_isotones_atingido
       character(len=2) :: elemento
       character(len=2), dimension(NUM_ELEMENTOS) :: elementos
       ! Calculate and display the number of neutrons
        neutrons = A - Z

       ! Display the element name
       elemento = elementos(Z)
       print *, "Elemento: ", trim(adjustl(elemento)), ", Número Atômico: ", Z, &
              &", Número de Massa: ", A, ", Nêutrons:", neutrons

       ! Calculate Isotopes (same element, different A)
       print *, " "
       print *, "Isótopos (mesmo elemento, diferentes números de massa):"
       do i = 1, 10
          if(A+i > Z) then
            print *, "Isótopo (A+): ", trim(adjustl(elemento)), "-", A + i
          end if
       end do
       do i = 1, 10
        if (A-i > Z) then
           print *, "Isótopo (A-): ", trim(adjustl(elemento)), "-", A - i
          end if
        end do

       ! Calculate Isobars (same mass number, different elements)
       print *, " "
       print *, "Isóbaros (mesmo número de massa, elementos diferentes):"
       contagem_isobaros = 0
       limite_isobaros_atingido = .false.

       do i = 1, NUM_ELEMENTOS
            if (i /= Z) then
              contagem_isobaros = contagem_isobaros + 1
               print *, "Isóbaro: ", trim(adjustl(elementos(i))), "-", A, ", Z=", i
            end if
             if (contagem_isobaros >= 10) then
               limite_isobaros_atingido = .true.
               exit
             end if
       end do
       if (limite_isobaros_atingido) then
         print *, "Nota: Apenas os 10 primeiros isóbaros são exibidos"
       end if

       ! Calculate Isotones (same number of neutrons, different elements)
       print *, " "
       print *, "Isótonos (mesmo número de nêutrons, elementos diferentes):"
       contagem_isotones = 0
       limite_isotones_atingido = .false.
       do isotone_Z = 1, NUM_ELEMENTOS
           if (isotone_Z /= Z) then
               neutrons_atuais = A - Z  ! Base neutrons
               isotone_A = isotone_Z + neutrons_atuais
               if (A - Z == isotone_A - isotone_Z) then  ! Comparing neutron numbers
                   contagem_isotones = contagem_isotones + 1
                   print *, "Isótono: ", trim(adjustl(elementos(isotone_Z))), "-", isotone_A, &
                       &", Z=", isotone_Z, ", nêutrons=", neutrons_atuais
               end if
           end if
           if (contagem_isotones >= 10) then
               limite_isotones_atingido = .true.
               exit
           end if
       end do
       if (limite_isotones_atingido) then
           print *, "Nota: Apenas os 10 primeiros isótonos são exibidos"
       end if
   end subroutine analyze_nuclide

end program isotope_isobar_isotone_analyzer