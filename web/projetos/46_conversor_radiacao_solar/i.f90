program conversor_radiacao_solar
    implicit none

    real :: cal_per_cm2_min, joules_per_m2_s
    real, parameter :: CAL_TO_JOULE = 4.184  
    real, parameter :: CM2_TO_M2 = 0.0001  
    real, parameter :: MIN_TO_SEC = 60.0     

    print *, "Conversão de Radiação Solar"
    print *, "Este programa converte radiação solar de cal/cm²·min para J/m²·s."
    print *, ""

    print *, "Digite o valor da radiação solar em cal/cm²·min:"
    read *, cal_per_cm2_min

    joules_per_m2_s = cal_per_cm2_min * CAL_TO_JOULE / (CM2_TO_M2 * MIN_TO_SEC)

    print '(A, F10.2)', "Valor equivalente em J/m²·s: ", joules_per_m2_s

end program conversor_radiacao_solar
