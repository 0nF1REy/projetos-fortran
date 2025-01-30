program estequiometria_AlOH3_Al2SO43
    implicit none
    real :: mols_aloh3, mols_al2so43
    character(len=1) :: continuar
    integer :: iostat

    print *, "==========================================="
    print *, " Cálculo de conversão de mols de Al(OH)3 "
    print *, " para mols de Al2(SO4)3 "
    print *, "==========================================="
    print *, "Este programa calcula quantos mols de Al2(SO4)3"
    print *, "são necessários para obter a quantidade desejada"
    print *, "de Al(OH)3, com base na relação estequiométrica:"
    print *, "2 mols de Al(OH)3 -> 1 mol de Al2(SO4)3"
    print *, "==========================================="

    do
        do
            print *, "Digite o número de mols de Al(OH)3 desejados (valor positivo):"
            read *, mols_aloh3

            if (mols_aloh3 < 0.0) then
                print *, "Erro: O número de mols não pode ser negativo. Tente novamente."
            else
                exit  
            end if
        end do

        ! Cálculo do número de mols de Al2(SO4)3 necessários
        mols_al2so43 = mols_aloh3 / 2.0

        if (mols_aloh3 == 0.0) then
            print *, "Nenhuma reação ocorrerá, pois não há mols de Al(OH)3."
        else if (mols_aloh3 < 1.0) then
            print '(A, F6.4)', "Pequena quantidade. Mols de Al2(SO4)3 necessários: ", mols_al2so43
        else if (mols_aloh3 > 100.0) then
            print '(A, F6.2)', "Grande quantidade! Mols de Al2(SO4)3 necessários: ", mols_al2so43
        else
            print '(A, F6.2)', "Mols de Al2(SO4)3 necessários: ", mols_al2so43
        end if

        do
            print *, "Deseja fazer outro cálculo? (S/N)"
            read *, continuar
            if (continuar == 'S' .or. continuar == 's' .or. continuar == 'N' .or. continuar == 'n') then
                exit
            else
                print *, "Entrada inválida. Digite apenas 'S' para continuar ou 'N' para sair."
            end if
        end do

        if (continuar == 'n' .or. continuar == 'N') exit
    end do

    print *, "Programa encerrado."
end program estequiometria_AlOH3_Al2SO43
