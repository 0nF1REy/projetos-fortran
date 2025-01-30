program combustao_metano_reacao
  implicit none

  real :: mols_ch4, mols_co2

  print *, "Digite o número de mols de metano (CH4):"
  read *, mols_ch4

  ! Cálculo do número de mols de CO2 com base na proporção 1:1
  mols_co2 = mols_ch4

  write(*, '(A,F6.2)') "Número de mols de CO2 produzidos: ", mols_co2
  
end program combustao_metano_reacao
