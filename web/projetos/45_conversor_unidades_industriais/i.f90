program conversor_unidades_industriais
    implicit none
    integer :: choice
    real(8) :: value, result
    real(8) :: hours, minutes

    ! Exibir opções
    print *, 'Programa de Conversão de Unidades para Processos Industriais'
    print *, 'Selecione uma opção:'
    print *, '1) Converter horas e minutos para segundos'
    print *, '2) Converter semanas para horas'
    print *, '3) Converter metros para hectômetros'
    print *, '4) Converter metros quadrados para centímetros quadrados'
    print *, '5) Converter decímetros quadrados para hectômetros quadrados'
    print *, '6) Converter centímetros cúbicos para metros cúbicos'
    print *, '7) Converter centímetros cúbicos para decímetros cúbicos'
    print *, '8) Converter quilômetros por hora para metros por segundo'
    print *, '9) Converter polegadas cúbicas por dia para centímetros cúbicos por minuto'
    print *, 'Digite sua escolha (1-9):'
    read *, choice

    ! Realizar a conversão selecionada
    select case(choice)
    case(1)
        print *, 'Digite o tempo em horas e minutos (formato: horas minutos):'
        read *, hours, minutes
        result = (hours * 3600.0) + (minutes * 60.0)
        print *, 'Resultado: ', result, 'segundos'

    case(2)
        print *, 'Digite o tempo em semanas:'
        read *, value
        result = value * 168.0
        print *, 'Resultado: ', result, 'horas'

    case(3)
        print *, 'Digite a distância em metros:'
        read *, value
        result = value / 10.0
        print *, 'Resultado: ', result, 'hectômetros'

    case(4)
        print *, 'Digite a área em metros quadrados:'
        read *, value
        result = value * 10000.0
        print *, 'Resultado: ', result, 'centímetros quadrados'

    case(5)
        print *, 'Digite a área em decímetros quadrados:'
        read *, value
        result = value / 10000.0
        print *, 'Resultado: ', result, 'hectômetros quadrados'

    case(6)
        print *, 'Digite o volume em centímetros cúbicos:'
        read *, value
        result = value / 1000000.0
        print *, 'Resultado: ', result, 'metros cúbicos'

    case(7)
        print *, 'Digite o volume em centímetros cúbicos:'
        read *, value
        result = value / 1000.0
        print *, 'Resultado: ', result, 'decímetros cúbicos'

    case(8)
        print *, 'Digite a velocidade em quilômetros por hora:'
        read *, value
        result = value * 1000.0 / 3600.0
        print *, 'Resultado: ', result, 'metros por segundo'

    case(9)
        print *, 'Digite o volume em polegadas cúbicas por dia:'
        read *, value
        result = value * 16.387064 / 1440.0
        print *, 'Resultado: ', result, 'centímetros cúbicos por minuto'

    case default
        print *, 'Escolha inválida!'
    end select

end program conversor_unidades_industriais