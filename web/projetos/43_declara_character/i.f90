program declara_character
    implicit none

    character :: cadeia_de_caracteres ! default: comprimento 1
    character(len = 30) :: string

    cadeia_de_caracteres = '!Minha mãe mandou eu escolher'
    print *, 'Tabela', cadeia_de_caracteres

    string = 'Alan Ryan da Silva Domingues'

    print *, '================================================================================================='
    print *, ' Indivíduo                      |   Primeiro nome   |   Nome do meio   |        Sobrenome    '
    print *, '-------------------------------------------------------------------------------------------------'
    print *, ' ', string, ' |        ', string(1:4), '       |      ', string(6:10), '       |   ', string(11:)
    print *, '================================================================================================='

end program declara_character
