program particle_velocity_calculator
    implicit none

    real :: rho_s, rho_l, D, g, n, v
    real :: rho_s_kgm3, rho_l_kgm3, D_m, g_m_s2, n_kg_m_s

    print *, 'Digite a densidade da partícula (g/cm³):'
    read *, rho_s

    print *, 'Digite a densidade do fluido (g/cm³):'
    read *, rho_l

    print *, 'Digite o diâmetro da partícula (cm):'
    read *, D

    print *, 'Digite a aceleração devido à gravidade (cm/s²):'
    read *, g

    print *, 'Digite a viscosidade do fluido (g/cm·s):'
    read *, n

    if (rho_s <= rho_l) then
        print *, 'Erro: A densidade da partícula deve ser maior que a densidade do fluido.'
        stop
    elseif (rho_s <= 0.0) then
        print *, 'Erro: A densidade da partícula deve ser maior que zero.'
        stop
    endif

    if (rho_l <= 0.0) then
        print *, 'Erro: A densidade do fluido deve ser maior que zero.'
        stop
    endif

    if (D <= 0.0) then
        print *, 'Erro: O diâmetro da partícula deve ser maior que zero.'
        stop
    endif

    if (n <= 0.0) then
        print *, 'Erro: A viscosidade do fluido deve ser maior que zero.'
        stop
    endif

    if (g <= 0.0) then
        print *, 'Erro: A aceleração devido à gravidade deve ser maior que zero.'
        stop
    endif

    rho_s_kgm3 = rho_s * 1000.0
    rho_l_kgm3 = rho_l * 1000.0

    D_m = D / 100.0
    g_m_s2 = g / 100.0
    n_kg_m_s = n * 0.001

    print *, 'Densidade da partícula em kg/m³: ', rho_s_kgm3
    print *, 'Densidade do fluido em kg/m³: ', rho_l_kgm3
    print *, 'Diâmetro da partícula em metros: ', D_m
    print *, 'Aceleração devido à gravidade em m/s²: ', g_m_s2
    print *, 'Viscosidade do fluido em kg/(m·s): ', n_kg_m_s

    ! Calculando a velocidade com base na fórmula de Stokes
    v = ((rho_s_kgm3 - rho_l_kgm3) * (D_m**2) * g_m_s2) / (18.0 * n_kg_m_s)

    print *, 'Valor calculado da velocidade (m/s): ', v

    write(*, '(A, F10.4, A)') 'A velocidade da partícula é: ', v, ' m/s'
    
end program particle_velocity_calculator