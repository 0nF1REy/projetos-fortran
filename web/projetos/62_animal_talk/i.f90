PROGRAM animal_talk
    IMPLICIT NONE
    CHARACTER(LEN=100) :: message
    INTEGER :: escolha

    PRINT *, ''
    PRINT *, '================================='
    PRINT *, 'Escolha um animal para falar:'
    PRINT *, '1 - Pinguim'
    PRINT *, '2 - Gato'
    PRINT *, '3 - Golfinho' 
    PRINT *, '-------------------------------'
    PRINT *, 'Digite o número da sua escolha:'
    READ *, escolha

    PRINT *, '-------------------------------'
    PRINT *, 'Forneça a mensagem:'
    READ(*,'(A)') message
    PRINT *, '== == == == == == == == == == == == =='
    PRINT *, ''
    SELECT CASE (escolha)
        CASE (1)
            CALL print_pinguim_balloon(message)
        CASE (2)
            CALL print_gato_balloon(message)
        CASE (3)
            CALL print_golfinho_balloon(message) 
        CASE DEFAULT
            PRINT *, 'Escolha inválida.'
    END SELECT
    PRINT *, '========================================='

END PROGRAM animal_talk

SUBROUTINE print_pinguim_balloon(message)
    IMPLICIT NONE
    CHARACTER(LEN=100), INTENT(IN) :: message
    INTEGER :: largura

    largura = LEN(TRIM(message)) + 4

    PRINT *, ' ' // REPEAT('-', largura)
    PRINT *, '< ' // TRIM(message) // ' >'
    PRINT *, ' ' // REPEAT('-', largura)

    PRINT *, '   \\'
    PRINT *, '    \\'

    PRINT *, '        .--.'
    PRINT*, '       |^_^ |'
    PRINT*, '       |    |'
    PRINT*, '      //   \ \'
    PRINT*, '     (|     | )'
    PRINT*, '    /\\_   _/`\\'
    PRINT*, '    \___)=(___/'

END SUBROUTINE print_pinguim_balloon

SUBROUTINE print_gato_balloon(message)
    IMPLICIT NONE
    CHARACTER(LEN=100), INTENT(IN) :: message
    INTEGER :: largura

    largura = LEN(TRIM(message)) + 4

    PRINT *, ' ' // REPEAT('-', largura)
    PRINT *, '< ' // TRIM(message) // ' >'
    PRINT *, ' ' // REPEAT('-', largura)

    PRINT *, '   \\'
    PRINT *, '    \\'

    PRINT *, '        _                        '
    PRINT *, '       \`*-.                    '
    PRINT *, '        )  _`-.                 '
    PRINT *, '       .  : `. .                '
    PRINT *, '       : _   ''  \               '
    PRINT *, '       ; *` _.   `*-._          '
    PRINT *, '       `-.-''          `-.       '
    PRINT *, '         ;       `       `.     '
    PRINT *, '         :.       .        \    '
    PRINT *, '         . \  .   :   .-''   .   '
    PRINT *, '         ''  `+.;  ;  ''      :   '
    PRINT *, '         :  ''  |    ;       ;-. '
    PRINT *, '         ; ''   : :`-:     _.`* ;'
    PRINT *, '   .*'' /  .*'' ; .*`- +''  ''`*'' '
    PRINT *, '        `*-*   `*-*  `*-*''       '

END SUBROUTINE print_gato_balloon

SUBROUTINE print_golfinho_balloon(message)
    IMPLICIT NONE
    CHARACTER(LEN=100), INTENT(IN) :: message
    INTEGER :: largura

    largura = LEN(TRIM(message)) + 4

    PRINT *, '             ' // REPEAT('-', largura)
    PRINT *, '            < ' // TRIM(message) // ' >'
    PRINT *, '             ' // REPEAT('-', largura)

    PRINT *, '               \\'
    PRINT *, '                \\'
    PRINT *, '                                   __'
    PRINT *, '                               _.-~  )'
    PRINT *, '                    _..--~~~~,''   ,-/     _'
    PRINT *, '                 .-''. . . .''   ,-','    ,'' )'
    PRINT *, '               ,''. . . _   ,--~,-''__..-''  ,'' '
    PRINT *, '             ,''. . .  (@)'' ---~~~~      ,'' '
    PRINT *, '            /. . . . ''~~             ,-'' '
    PRINT *, '           /. . . . .             ,-'' '
    PRINT *, '          ; . . . .  - .        ,'' '
    PRINT *, '         : . . . .       _     / '
    PRINT *, '          ; . . . .  - .        ,'' '
    PRINT *, '         : . . . .       _     / '
    PRINT *, '        . . . . .          `-.:'
    PRINT *, '       . . . ./  - .          )'
    PRINT *, '      .  . . |  _____..---.._/ ____ _'

END SUBROUTINE print_golfinho_balloon