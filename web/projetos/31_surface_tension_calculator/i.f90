program surface_tension_calculator
    implicit none
    real(8) :: massa_total, aceleracao_gravidade, fator_correcao, raio_tubo, tensao_superficial
    real(8) :: massa_a, massa_b, tensao_superficial_a, tensao_superficial_b
    integer :: num_gotas, opcao
    real(8), parameter :: pi = 3.141592653589793_8

    print*, "Digite a massa total das gotas (em kg):"
    read*, massa_total
    if (massa_total <= 0.0_8) then
        print*, "Erro: A massa total das gotas deve ser positiva."
        stop
    end if

    print*, "Digite o número de gotas:"
    read*, num_gotas
    if (num_gotas <= 0) then
        print*, "Erro: O número de gotas deve ser um valor positivo."
        stop
    end if

    print*, "Digite a aceleração da gravidade (m/s²):"
    read*, aceleracao_gravidade
    if (aceleracao_gravidade <= 0.0_8) then
        print*, "Erro: A aceleração da gravidade deve ser um valor positivo."
        stop
    end if

    print*, "Digite o fator de correção (sugestão: 0.6):"
    read*, fator_correcao
    if (fator_correcao <= 0.0_8) then
        print*, "Erro: O fator de correção deve ser um valor positivo."
        stop
    end if

    print*, "Digite o raio do tubo (em metros):"
    read*, raio_tubo
    if (raio_tubo <= 0.0_8) then
        print*, "Erro: O raio do tubo deve ser um valor positivo."
        stop
    end if

    tensao_superficial = massa_total * aceleracao_gravidade / (real(num_gotas, kind=8) * 2.0_8 * pi * raio_tubo * fator_correcao)
    print "(A,F15.8,A)", "A tensão superficial do líquido estudado é: ", tensao_superficial, " N/m" 

    ! Opção para calcular a tensão superficial de outro líquido comparado ao líquido de referência
    print*, "Deseja calcular a tensão superficial de outro líquido comparado a um líquido de referência? (1 = Sim, 0 = Não)"
    read*, opcao
    if (opcao == 1) then
        print*, "Digite a massa total das gotas do líquido de referência (em kg):"
        read*, massa_a
        if (massa_a <= 0.0_8) then
            print*, "Erro: A massa do líquido de referência deve ser positiva."
            stop
        end if

        print*, "Digite a tensão superficial do líquido de referência (N/m):"
        read*, tensao_superficial_a
        if (tensao_superficial_a <= 0.0_8) then
            print*, "Erro: A tensão superficial do líquido de referência deve ser positiva."
            stop
        end if

        print*, "Digite a massa total das gotas do líquido desconhecido (em kg):"
        read*, massa_b
        if (massa_b <= 0.0_8) then
            print*, "Erro: A massa do líquido desconhecido deve ser positiva."
            stop
        end if

        ! Cálculo da tensão superficial do líquido desconhecido
        tensao_superficial_b = (massa_b * tensao_superficial_a) / massa_a
        print "(A,F15.8,A)", "A tensão superficial do líquido desconhecido é: ", tensao_superficial_b, " N/m"

        ! Verificação de valores inconsistentes
        if (tensao_superficial_b > 1.0_8) then
            print*, "Atenção: a tensão superficial calculada é muito alta e pode indicar valores de entrada inconsistentes."
        end if
    endif

end program surface_tension_calculator