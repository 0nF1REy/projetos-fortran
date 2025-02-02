program tabela_periodica
    implicit none

    type :: Element
        integer :: atomic_number
        real :: atomic_mass
        character(len=3) :: symbol
        character(len=20) :: name
        character(len=25) :: category
        real :: electronegativity
        real :: atomic_radius
    end type Element

    type(Element), allocatable :: elements(:)
    integer :: choice
    character(len=3) :: search_symbol
    integer :: search_number
    integer :: num_elements
    character(len=25) :: search_category

    ! Inicializa o número de elementos na tabela periódica
    num_elements = 118

    ! Aloca o array de elementos
    allocate(elements(num_elements))

    ! Inicializa os elementos da tabela periódica
    call initialize_elements(elements)

    ! Exibe o menu de opções
    do
        print *, " "
        print *, "Escolha uma opção:"
        print *, "1 - Exibir Tabela Periódica"
        print *, "2 - Buscar por Símbolo"
        print *, "3 - Buscar por Número Atômico"
        print *, "4 - Buscar por Categoria"
        print *, "5 - Categorizar Elementos"
        print *, "6 - Ordenar por Número Atômico"
        print *, "0 - Sair"
        print *, " "
        read *, choice

        select case (choice)
        case (1)
            call display_periodic_table(elements)
        case (2)
            print *, "Digite o símbolo do elemento (ex: H, He, Li):"
            read *, search_symbol
            call search_by_symbol(elements, search_symbol)
        case (3)
            print *, "Digite o número atômico do elemento:"
            read *, search_number
            call search_by_atomic_number(elements, search_number)
        case (4)
            print *, "Digite a categoria para busca (ex: Alkali Metal, Halogen):"
            read *, search_category
            call search_by_category(elements, search_category)
         case (5)
            call categorize_elements(elements)
        case (6)
            call sort_by_atomic_number(elements)
            print *, "Tabela ordenada por número atômico!"
        case (0)
            print *, "Saindo..."
            exit
        case default
            print *, "Opção inválida. Tente novamente."
        end select
    end do
    deallocate(elements)
contains
    subroutine initialize_elements(elements)
        type(Element), dimension(:), intent(inout) :: elements

        ! Inicializa os elementos da tabela periódica
        elements(1) = Element(1, 1.008, 'H', 'Hydrogen', 'Nonmetal', 2.20, 53)
        elements(2) = Element(2, 4.0026, 'He', 'Helium', 'Noble Gas', 0.0, 31)
        elements(3) = Element(3, 6.94, 'Li', 'Lithium', 'Alkali Metal', 0.98, 167)
        elements(4) = Element(4, 9.0122, 'Be', 'Beryllium', 'Alkaline Earth Metal', 1.57, 112)
        elements(5) = Element(5, 10.81, 'B', 'Boron', 'Metalloid', 2.04, 87)
        elements(6) = Element(6, 12.011, 'C', 'Carbon', 'Nonmetal', 2.55, 67)
        elements(7) = Element(7, 14.007, 'N', 'Nitrogen', 'Nonmetal', 3.04, 56)
        elements(8) = Element(8, 15.999, 'O', 'Oxygen', 'Nonmetal', 3.44, 48)
        elements(9) = Element(9, 18.998, 'F', 'Fluorine', 'Halogen', 3.98, 42)
        elements(10) = Element(10, 20.180, 'Ne', 'Neon', 'Noble Gas', 0.0, 38)
        elements(11) = Element(11, 22.990, 'Na', 'Sodium', 'Alkali Metal', 0.93, 190)
        elements(12) = Element(12, 24.305, 'Mg', 'Magnesium', 'Alkaline Earth Metal', 1.31, 145)
        elements(13) = Element(13, 26.982, 'Al', 'Aluminum', 'Post-transition Metal', 1.61, 118)
        elements(14) = Element(14, 28.085, 'Si', 'Silicon', 'Metalloid', 1.90, 111)
        elements(15) = Element(15, 30.974, 'P', 'Phosphorus', 'Nonmetal', 2.19, 98)
        elements(16) = Element(16, 32.06, 'S', 'Sulfur', 'Nonmetal', 2.58, 88)
        elements(17) = Element(17, 35.45, 'Cl', 'Chlorine', 'Halogen', 3.16, 79)
        elements(18) = Element(18, 39.948, 'Ar', 'Argon', 'Noble Gas', 0.0, 71)
        elements(19) = Element(19, 39.098, 'K', 'Potassium', 'Alkali Metal', 0.82, 243)
        elements(20) = Element(20, 40.078, 'Ca', 'Calcium', 'Alkaline Earth Metal', 1.00, 194)
        elements(21) = Element(21, 44.956, 'Sc', 'Scandium', 'Transition Metal', 1.36, 184)
        elements(22) = Element(22, 47.867, 'Ti', 'Titanium', 'Transition Metal', 1.54, 176)
        elements(23) = Element(23, 50.941, 'V', 'Vanadium', 'Transition Metal', 1.63, 171)
        elements(24) = Element(24, 51.996, 'Cr', 'Chromium', 'Transition Metal', 1.66, 166)
        elements(25) = Element(25, 54.938, 'Mn', 'Manganese', 'Transition Metal', 1.55, 161)
        elements(26) = Element(26, 55.845, 'Fe', 'Iron', 'Transition Metal', 1.83, 156)
        elements(27) = Element(27, 58.933, 'Co', 'Cobalt', 'Transition Metal', 1.88, 152)
        elements(28) = Element(28, 58.693, 'Ni', 'Nickel', 'Transition Metal', 1.91, 149)
        elements(29) = Element(29, 63.546, 'Cu', 'Copper', 'Transition Metal', 1.90, 145)
        elements(30) = Element(30, 65.38, 'Zn', 'Zinc', 'Transition Metal', 1.65, 139)
        elements(31) = Element(31, 69.723, 'Ga', 'Gallium', 'Post-transition Metal', 1.81, 136)
        elements(32) = Element(32, 72.630, 'Ge', 'Germanium', 'Metalloid', 2.01, 125)
        elements(33) = Element(33, 74.921, 'As', 'Arsenic', 'Metalloid', 2.18, 114)
        elements(34) = Element(34, 78.971, 'Se', 'Selenium', 'Nonmetal', 2.55, 103)
        elements(35) = Element(35, 79.904, 'Br', 'Bromine', 'Halogen', 2.96, 94)
        elements(36) = Element(36, 83.798, 'Kr', 'Krypton', 'Noble Gas', 0.0, 88)
        elements(37) = Element(37, 85.468, 'Rb', 'Rubidium', 'Alkali Metal', 0.82, 265)
        elements(38) = Element(38, 87.62, 'Sr', 'Strontium', 'Alkaline Earth Metal', 0.95, 219)
        elements(39) = Element(39, 88.906, 'Y', 'Yttrium', 'Transition Metal', 1.22, 212)
        elements(40) = Element(40, 91.224, 'Zr', 'Zirconium', 'Transition Metal', 1.33, 206)
        elements(41) = Element(41, 92.906, 'Nb', 'Niobium', 'Transition Metal', 1.6, 198)
        elements(42) = Element(42, 95.95, 'Mo', 'Molybdenum', 'Transition Metal', 2.16, 190)
        elements(43) = Element(43, 98, 'Tc', 'Technetium', 'Transition Metal', 1.9, 183)
        elements(44) = Element(44, 101.07, 'Ru', 'Ruthenium', 'Transition Metal', 2.2, 178)
        elements(45) = Element(45, 102.91, 'Rh', 'Rhodium', 'Transition Metal', 2.28, 173)
        elements(46) = Element(46, 106.42, 'Pd', 'Palladium', 'Transition Metal', 2.20, 169)
        elements(47) = Element(47, 107.87, 'Ag', 'Silver', 'Transition Metal', 1.93, 165)
        elements(48) = Element(48, 112.41, 'Cd', 'Cadmium', 'Transition Metal', 1.69, 161)
        elements(49) = Element(49, 114.82, 'In', 'Indium', 'Post-transition Metal', 1.78, 156)
        elements(50) = Element(50, 118.71, 'Sn', 'Tin', 'Post-transition Metal', 1.96, 145)
        elements(51) = Element(51, 121.76, 'Sb', 'Antimony', 'Metalloid', 2.05, 133)
        elements(52) = Element(52, 127.60, 'Te', 'Tellurium', 'Metalloid', 2.10, 123)
        elements(53) = Element(53, 126.90, 'I', 'Iodine', 'Halogen', 2.66, 115)
        elements(54) = Element(54, 131.29, 'Xe', 'Xenon', 'Noble Gas', 0.0, 108)
        elements(55) = Element(55, 132.91, 'Cs', 'Cesium', 'Alkali Metal', 0.79, 298)
        elements(56) = Element(56, 137.33, 'Ba', 'Barium', 'Alkaline Earth Metal', 0.89, 253)
        elements(57) = Element(57, 138.91, 'La', 'Lanthanum', 'Lanthanide', 1.10, 214)
        elements(58) = Element(58, 140.12, 'Ce', 'Cerium', 'Lanthanide', 1.12, 207)
        elements(59) = Element(59, 140.91, 'Pr', 'Praseodymium', 'Lanthanide', 1.13, 203)
        elements(60) = Element(60, 144.24, 'Nd', 'Neodymium', 'Lanthanide', 1.14, 201)
        elements(61) = Element(61, 145, 'Pm', 'Promethium', 'Lanthanide', 0.0, 199)
        elements(62) = Element(62, 150.36, 'Sm', 'Samarium', 'Lanthanide', 1.17, 198)
        elements(63) = Element(63, 151.96, 'Eu', 'Europium', 'Lanthanide', 0.0, 197)
        elements(64) = Element(64, 157.25, 'Gd', 'Gadolinium', 'Lanthanide', 1.20, 196)
        elements(65) = Element(65, 158.93, 'Tb', 'Terbium', 'Lanthanide', 0.0, 194)
        elements(66) = Element(66, 162.50, 'Dy', 'Dysprosium', 'Lanthanide', 1.22, 192)
        elements(67) = Element(67, 164.93, 'Ho', 'Holmium', 'Lanthanide', 1.23, 192)
        elements(68) = Element(68, 167.26, 'Er', 'Erbium', 'Lanthanide', 1.24, 189)
        elements(69) = Element(69, 168.93, 'Tm', 'Thulium', 'Lanthanide', 0.0, 190)
        elements(70) = Element(70, 173.05, 'Yb', 'Ytterbium', 'Lanthanide', 0.0, 194)
        elements(71) = Element(71, 174.97, 'Lu', 'Lutetium', 'Lanthanide', 1.27, 187)
        elements(72) = Element(72, 178.49, 'Hf', 'Hafnium', 'Transition Metal', 1.3, 167)
        elements(73) = Element(73, 180.95, 'Ta', 'Tantalum', 'Transition Metal', 1.5, 172)
        elements(74) = Element(74, 183.84, 'W', 'Tungsten', 'Transition Metal', 2.36, 179)
        elements(75) = Element(75, 186.21, 'Re', 'Rhenium', 'Transition Metal', 1.9, 188)
        elements(76) = Element(76, 190.23, 'Os', 'Osmium', 'Transition Metal', 2.2, 191)
        elements(77) = Element(77, 192.22, 'Ir', 'Iridium', 'Transition Metal', 2.20, 195)
        elements(78) = Element(78, 195.08, 'Pt', 'Platinum', 'Transition Metal', 2.28, 197)
        elements(79) = Element(79, 196.97, 'Au', 'Gold', 'Transition Metal', 2.54, 199)
        elements(80) = Element(80, 200.59, 'Hg', 'Mercury', 'Transition Metal', 2.00, 150)
        elements(81) = Element(81, 204.38, 'Tl', 'Thallium', 'Post-transition Metal', 2.04, 155)
        elements(82) = Element(82, 207.2, 'Pb', 'Lead', 'Post-transition Metal', 2.33, 154)
        elements(83) = Element(83, 208.98, 'Bi', 'Bismuth', 'Post-transition Metal', 2.02, 163)
        elements(84) = Element(84, 209, 'Po', 'Polonium', 'Metalloid', 2.0, 167)
        elements(85) = Element(85, 210, 'At', 'Astatine', 'Halogen', 2.2, 170)
        elements(86) = Element(86, 222, 'Rn', 'Radon', 'Noble Gas', 0.0, 120)
        elements(87) = Element(87, 223, 'Fr', 'Francium', 'Alkali Metal', 0.7, 260)
        elements(88) = Element(88, 226, 'Ra', 'Radium', 'Alkaline Earth Metal', 0.9, 221)
        elements(89) = Element(89, 227, 'Ac', 'Actinium', 'Actinide', 1.1, 203)
        elements(90) = Element(90, 232.04, 'Th', 'Thorium', 'Actinide', 1.3, 206)
        elements(91) = Element(91, 231.04, 'Pa', 'Protactinium', 'Actinide', 1.5, 201)
        elements(92) = Element(92, 238.03, 'U', 'Uranium', 'Actinide', 1.38, 196)
        elements(93) = Element(93, 237, 'Np', 'Neptunium', 'Actinide', 1.36, 190)
        elements(94) = Element(94, 244, 'Pu', 'Plutonium', 'Actinide', 1.28, 188)
        elements(95) = Element(95, 243, 'Am', 'Americium', 'Actinide', 1.13, 184)
        elements(96) = Element(96, 247, 'Cm', 'Curium', 'Actinide', 1.28, 184)
        elements(97) = Element(97, 247, 'Bk', 'Berkelium', 'Actinide', 1.3, 180)
        elements(98) = Element(98, 251, 'Cf', 'Californium', 'Actinide', 1.3, 179)
        elements(99) = Element(99, 252, 'Es', 'Einsteinium', 'Actinide', 1.3, 178)
        elements(100) = Element(100, 257, 'Fm', 'Fermium', 'Actinide', 1.3, 177)
        elements(101) = Element(101, 258, 'Md', 'Mendelevium', 'Actinide', 1.3, 173)
        elements(102) = Element(102, 259, 'No', 'Nobelium', 'Actinide', 1.3, 172)
        elements(103) = Element(103, 266, 'Lr', 'Lawrencium', 'Actinide', 0.0, 171)
        elements(104) = Element(104, 267, 'Rf', 'Rutherfordium', 'Transition Metal', 0.0, 170)
        elements(105) = Element(105, 268, 'Db', 'Dubnium', 'Transition Metal', 0.0, 167)
        elements(106) = Element(106, 269, 'Sg', 'Seaborgium', 'Transition Metal', 0.0, 164)
        elements(107) = Element(107, 270, 'Bh', 'Bohrium', 'Transition Metal', 0.0, 162)
        elements(108) = Element(108, 277, 'Hs', 'Hassium', 'Transition Metal', 0.0, 159)
        elements(109) = Element(109, 278, 'Mt', 'Meitnerium', 'Transition Metal', 0.0, 157)
        elements(110) = Element(110, 281, 'Ds', 'Darmstadtium', 'Transition Metal', 0.0, 156)
        elements(111) = Element(111, 282, 'Rg', 'Roentgenium', 'Transition Metal', 0.0, 155)
        elements(112) = Element(112, 285, 'Cn', 'Copernicium', 'Transition Metal', 0.0, 151)
        elements(113) = Element(113, 286, 'Nh', 'Nihonium', 'Post-transition Metal', 0.0, 150)
        elements(114) = Element(114, 289, 'Fl', 'Flerovium', 'Post-transition Metal', 0.0, 147)
        elements(115) = Element(115, 289, 'Mc', 'Moscovium', 'Post-transition Metal', 0.0, 145)
        elements(116) = Element(116, 293, 'Lv', 'Livermorium', 'Post-transition Metal', 0.0, 142)
        elements(117) = Element(117, 294, 'Ts', 'Tennessine', 'Halogen', 0.0, 140)
        elements(118) = Element(118, 294, 'Og', 'Oganesson', 'Noble Gas', 0.0, 138)


    end subroutine initialize_elements

    subroutine display_periodic_table(elements)
        type(Element), dimension(:), intent(in) :: elements
        integer :: i
       
        character(len=130) :: separator
        separator = repeat("-", 130)

        print *, " "
        print *, separator
        print *, "  #  | Symbol |     Name     |     Category      |  Mass    | Electronegativity | Atomic Radius |"
        print *, separator

        do i = 1, size(elements)
            write(*, "(i3, 2x, a3, 2x, a20, 2x, a25, 2x, f8.4, 2x, f7.2, 2x, f10.1)") &
               elements(i)%atomic_number,  elements(i)%symbol, elements(i)%name,  elements(i)%category, &
               elements(i)%atomic_mass,  elements(i)%electronegativity, elements(i)%atomic_radius
        end do
         print *, separator
         print *, " "

    end subroutine display_periodic_table

     subroutine search_by_symbol(elements, symbol)
        type(Element), dimension(:), intent(in) :: elements
        character(len=3), intent(in) :: symbol
        integer :: i, found

        found = 0
        do i = 1, size(elements)
            if (trim(adjustl(elements(i)%symbol)) == trim(adjustl(symbol))) then
                print *, " "
                print *, "Elemento encontrado:"
                print *, "Número Atômico: ", elements(i)%atomic_number
                print *, "Nome: ", elements(i)%name
                print *, "Categoria: ", elements(i)%category
                print *, "Massa Atômica: ", elements(i)%atomic_mass
                 print *, "Eletronegatividade: ", elements(i)%electronegativity
                 print *, "Raio Atômico: ", elements(i)%atomic_radius
                 print *, " "
                found = 1
                exit
            end if
        end do

        if (found == 0) then
            print *, "Elemento com símbolo ", symbol, " não encontrado."
        end if
    end subroutine search_by_symbol

    subroutine search_by_atomic_number(elements, atomic_number)
        type(Element), dimension(:), intent(in) :: elements
        integer, intent(in) :: atomic_number
        integer :: i, found

        found = 0
        do i = 1, size(elements)
            if (elements(i)%atomic_number == atomic_number) then
                 print *, " "
                print *, "Elemento encontrado:"
                print *, "Símbolo: ", elements(i)%symbol
                print *, "Nome: ", elements(i)%name
                print *, "Categoria: ", elements(i)%category
                print *, "Massa Atômica: ", elements(i)%atomic_mass
                 print *, "Eletronegatividade: ", elements(i)%electronegativity
                  print *, "Raio Atômico: ", elements(i)%atomic_radius
                print *, " "
                found = 1
                exit
            end if
        end do

        if (found == 0) then
            print *, "Elemento com número atômico ", atomic_number, " não encontrado."
        end if
    end subroutine search_by_atomic_number

    subroutine search_by_category(elements, category)
        type(Element), dimension(:), intent(in) :: elements
         character(len=25), intent(in) :: category
         integer :: i, found

        found = 0
         print *, " "
         print *, "Elementos encontrados na categoria ", category, ":"

        do i = 1, size(elements)
          if (trim(adjustl(elements(i)%category)) == trim(adjustl(category))) then
            print *, "   Símbolo: ", elements(i)%symbol
            print *, "   Nome: ", elements(i)%name
             print *, "   Número Atômico: ", elements(i)%atomic_number
             print *, "   Massa Atômica: ", elements(i)%atomic_mass
              print *, "   Eletronegatividade: ", elements(i)%electronegativity
                 print *, "   Raio Atômico: ", elements(i)%atomic_radius
               print *, " -----------------------"
              found = 1
             end if
         end do

         if (found == 0) then
            print *, "Nenhum elemento encontrado na categoria ", category, "."
        end if
         print *, " "
     end subroutine search_by_category

    subroutine sort_by_atomic_number(elements)
        type(Element), dimension(:), intent(inout) :: elements
        type(Element) :: temp
        integer :: i, j

        do i = 1, size(elements) - 1
            do j = i + 1, size(elements)
                if (elements(i)%atomic_number > elements(j)%atomic_number) then
                    temp = elements(i)
                    elements(i) = elements(j)
                    elements(j) = temp
                end if
            end do
        end do
    end subroutine sort_by_atomic_number

   subroutine categorize_elements(elements)
        type(Element), dimension(:), intent(in) :: elements
        integer :: i

        print *, "Categorizing Elements:"

        do i = 1, size(elements)
             select case (elements(i)%category)
               case ('Noble Gas')
                print *, "Gás Nobre: ", elements(i)%name
               case ('Alkali Metal')
                print *, "Metal Alcalino: ", elements(i)%name
                case ('Alkaline Earth Metal')
                 print *, "Metal Alcalino Terroso: ", elements(i)%name
               case ('Halogen')
                print *, "Halogênio: ", elements(i)%name
              case ('Metalloid')
                print *, "Metaloide: ", elements(i)%name  
               case ('Nonmetal')
                print *, "Não Metal: ", elements(i)%name
              case ('Transition Metal')
                print *, "Metal de Transição: ", elements(i)%name
             case ('Post-transition Metal')
                print *, "Metal de Transição Pós: ", elements(i)%name
              case ('Lanthanide')
                print *, "Lantanídeo: ", elements(i)%name
              case ('Actinide')
                print *, "Actinídeo: ", elements(i)%name
              case default
                print *, "Categoria desconhecida: ", elements(i)%name
            end select
        end do
    end subroutine categorize_elements

end program tabela_periodica