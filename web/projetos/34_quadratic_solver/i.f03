program quadratic_solver
  implicit none

  real :: a, b, c
  real :: disc, raiz1, raiz2
  real, parameter :: epsilon = 1.0e-6  ! Tolerance for comparing real numbers

  complex, allocatable :: raizes_complexas(:)

  ! Handle the case a=0
  if (obter_coeficientes(a, b, c) /= 0 ) stop ! Stop if error in reading the coefficients

  if (abs(a) < epsilon) then
     if (handle_linear_case(b, c, epsilon) /= 0) stop ! Stop if linear case is not resolvable
     stop
  end if

  disc = b**2 - 4.0*a*c  ! Discriminant

  if (disc < -epsilon) then
    raizes_complexas = calcular_raizes_complexas(cmplx(a), cmplx(b), cmplx(c))
    print *, 'As raízes complexas são: ', raizes_complexas(1), ' e ', raizes_complexas(2)
  else
     call handle_real_roots(a,b,disc, epsilon)
  end if

contains

  !=========================================================
  ! Subroutine to obtain coefficients and handle errors
  ! Returns: 0 on sucess, 1 on error
  !=========================================================
  function obter_coeficientes(a, b, c) result(status)
    implicit none
    real, intent(out) :: a, b, c
    character(len=256) :: linha_entrada
    integer :: ios, num_tokens, i
    integer :: status
    
    status = 1

    do
        print *, 'Insira valores para a, b e c separados por vírgulas ou espaços:'
        read(*, '(A)', iostat=ios) linha_entrada

        if (ios /= 0) then
            print *, "Erro de leitura. Por favor, tente novamente."
            cycle  ! Back to the beginning of the loop
        end if
        
        ! Read the coefficients
        num_tokens = 0
        i = 1
        
        do 
            ! Skip spaces and commas until a number is found
            do while (i <= len(linha_entrada) .and. (linha_entrada(i:i) == ' ' .or. linha_entrada(i:i) == ','))
              i = i + 1
            end do
            
            if (i <= len(linha_entrada)) then
                num_tokens = num_tokens + 1
                
             ! Advance the read marker to the next number or end of the line
             do while (i <= len(linha_entrada) .and. (linha_entrada(i:i) /= ' ' .and. linha_entrada(i:i) /= ','))
                  i = i + 1
             end do
            else 
              exit
            end if
            
        end do

       if(num_tokens /= 3) then
          print *, "Por favor, insira 3 números."
           cycle
        end if
    
        i = 1
        read(linha_entrada, *, iostat=ios) a, b, c
        if (ios /= 0) then
           print *, "Entrada inválida. Por favor insira três números."
          cycle
        end if
            
        exit
    end do
     status = 0
  end function obter_coeficientes

  !=========================================================
  ! Subroutine to handle the linear case (a=0)
  ! Returns: 0 on success, 1 on error
  !=========================================================
   function handle_linear_case(b, c, epsilon) result(status)
    implicit none
    real, intent(in) :: b, c, epsilon
    real :: raiz1
    integer :: status
    
    status = 1

    if (abs(b) < epsilon) then
        if (abs(c) < epsilon) then
           print *, "Soluções infinitas: a, b e c são todas zero."
        else
          print *, "Nenhuma solução: a e b são zero, mas c não é."
        end if
    else
       print *, "Caso de equação linear."
       raiz1 = -c / b
       print *, 'A raiz única é: ', raiz1
       status = 0
    end if
    
  end function handle_linear_case

  !=========================================================
  ! Subroutine to handle the real roots case
  !=========================================================
  subroutine handle_real_roots(a,b, disc, epsilon)
    implicit none
    real, intent(in) :: a,b, disc, epsilon
    real :: raiz1, raiz2
     raiz1 = (-b + sqrt(disc))/(2.0*a)
      raiz2 = (-b - sqrt(disc))/(2.0*a)
      if( abs(raiz1 - raiz2) < epsilon) then
          print *, "Raiz repetida: ", raiz1
      else
         print *, 'As raízes reais são: ', raiz1, ' e ', raiz2
      end if
  end subroutine handle_real_roots
  
  !=========================================================
  ! Function to calculate complex roots
  !=========================================================
  function calcular_raizes_complexas(a,b,c) result(raizes)
    implicit none
    complex, intent(in) :: a, b, c
    complex, allocatable :: raizes(:)
    complex :: disc_complexo, raiz1_complexa, raiz2_complexa
    
    allocate(raizes(2))
    
    disc_complexo = b**2 - 4.0*a*c
    raiz1_complexa = (-b + sqrt(disc_complexo))/(2.0*a)
    raiz2_complexa = (-b - sqrt(disc_complexo))/(2.0*a)
    
    raizes(1) = raiz1_complexa
    raizes(2) = raiz2_complexa
  end function calcular_raizes_complexas

end program quadratic_solver