program conversor_ascii
    implicit none
    integer :: ascii_value, ios
    character(1) :: char_val
    character(len=100) :: input_string
    integer :: i, num_chars
    logical :: is_number

    print *, 'Digite um valor ASCII (entre 0 e 127), um caractere ou "tabela" para ver a tabela ASCII:'
    read(*,'(A)') input_string

    input_string = adjustl(input_string)
    num_chars = len_trim(input_string)

    if (trim(input_string) == 'tabela') then
        call exibir_tabela_ascii()
        stop
    end if

    if (num_chars == 0) then
        print *, 'Entrada vazia. Por favor, digite algo.'
        stop
    end if

    is_number = .true.
    do i = 1, num_chars
        if (input_string(i:i) < '0' .or. input_string(i:i) > '9') then
            is_number = .false.
            exit
        end if
    end do

    if (is_number) then
        read(input_string, *, iostat=ios) ascii_value
        if (ios /= 0) then
            print *, 'Entrada inválida. Por favor, digite um número ASCII válido.'
            stop
        end if

        if (ascii_value >= 0 .and. ascii_value <= 127) then
            char_val = achar(ascii_value)
            print *, 'Valor ASCII:', ascii_value
            print *, 'Caractere correspondente:', char_val
        else
            print *, 'Valor ASCII fora do intervalo válido (0 a 127).'
        end if
    else
        if (num_chars == 1) then
            char_val = input_string(1:1)
            ascii_value = iachar(char_val)
            print *, 'Caractere digitado:', char_val
            print *, 'Valor ASCII correspondente:', ascii_value
        else
            print *, 'Entrada inválida. Por favor, digite apenas um caractere ou um número ASCII válido.'
        end if
    end if

contains

    subroutine exibir_tabela_ascii()
        integer :: i
        print *, ''
        print *, '╔═════╦═════╦─────╗'
        print *, '║ DEC │ HEX │ CHR ║'
        print *, '╠═════╬═════╬─────╣'
        do i = 0, 127
            if (i >= 32 .and. i <= 126) then
                print '(A,I3,A,Z2,A,A1,A)', '║ ', i, ' │ ', i, ' │  ', achar(i), '  ║'
            else
                print '(A,I3,A,Z2,A,A,A)', '║ ', i, ' │ ', i, ' │ [CTRL] ║'
            end if
        end do
        print *, '╚═════╩═════╩─────╝'
    end subroutine exibir_tabela_ascii

end program conversor_ascii