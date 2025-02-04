program conversor_taxa_mol
  implicit none

  double precision :: input_rate
  double precision :: rate_mol_ft3_day     
  double precision :: rate_lbmol_ft3_day   
  character(len=3)  :: resposta
  character(len=20) :: substancia
  integer           :: substancia_id

  ! Constantes de conversão
  double precision, parameter :: conv_umol_to_mol = 1.0e-6        
  double precision, parameter :: conv_mL_to_ft3   = 3.53147e-5     
  double precision, parameter :: conv_min_to_day  = 1440.0        
  double precision, parameter :: mol_per_lbmol    = 453.59237     
  double precision, parameter :: conv_glucosa = conv_umol_to_mol * conv_mL_to_ft3 * conv_min_to_day
  
  ! Outras substâncias (se necessário, adicione mais aqui)
  double precision, parameter :: conv_sacarose = 1.0e-6 * 3.53147e-5 * 1440.0 ! Exemplo de conversão para sacarose
  
  print *, 'Escolha a substância (1: Glicose, 2: Sacarose, etc.):'
  print *, '1. Glicose'
  print *, '2. Sacarose'
  read *, substancia_id

  ! Validação da escolha da substância
  if (substancia_id < 1 .or. substancia_id > 2) then
     print *, 'Opção inválida. O programa será encerrado.'
     stop
  end if

  ! Solicitar a taxa de produção em μmol/(mL·min)
  print *, 'Insira a taxa de produção da substância escolhida em μmol/(mL·min):'
  read *, input_rate

  ! Validação da entrada da taxa
  if (input_rate <= 0.0) then
     print *, 'Erro: A taxa de produção deve ser maior que zero.'
     stop
  end if

  ! Seleção da substância e cálculo da taxa
  if (substancia_id == 1) then
     rate_mol_ft3_day = input_rate * conv_glucosa
     rate_lbmol_ft3_day = rate_mol_ft3_day / mol_per_lbmol
     substancia = 'Glicose'
  elseif (substancia_id == 2) then
     rate_mol_ft3_day = input_rate * conv_sacarose
     rate_lbmol_ft3_day = rate_mol_ft3_day / mol_per_lbmol
     substancia = 'Sacarose'
  end if

  ! Exibir resultado
  print *
  print *, 'Taxa de produção de ', substancia, ':'
  print *, 'Em lbmol/(ft³·dia): ', rate_lbmol_ft3_day

  ! Opção de exibir em notação científica
  print *, 'Deseja o resultado em notação científica (S/n)?'
  read *, resposta

  if (resposta == 's' .or. resposta == 'S' .or. resposta == 'y') then
    print *, 'Taxa de produção de ', substancia, ' (notação científica):'
    ! Formatação da notação científica com 3 casas decimais
    print '(ES12.3E2)', rate_lbmol_ft3_day
  end if

end program conversor_taxa_mol
