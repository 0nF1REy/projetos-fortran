program esfera

integer i,j,k               ! i,j,k = variáveis inteiras utilizadas em loops
real, allocatable :: X(:)   ! Matriz posição (x,y,z) da partícula
real, allocatable :: U(:)   ! Matriz velocidade (x,y,z) da partícula
real, allocatable :: T(:)   ! Vetor tempo

real dt                     ! dt = passo de tempo

integer npast               ! Número de passos de tempo
real k1,k2,k3,k4            ! Constantes utilizadas no Runge-Kutta de 4° ordem
real St                     ! Número de Stokes

! Números adimensionais
write(*,*) 'Qual o valor do número de Stokes?'
read*,St

! Número de passos de tempo e passo de tempo

npast=10000
dt=0.01

! Alocando variáveis na memória

allocate(X(3))
allocate(U(3))
allocate(T(npast+1))

! Velocidade inicial da partícula

U(1)=0.0
U(2)=0.0
U(3)=0.0


! Escrevendo a solução analítica para comparação

open (1,file='saida.plt')
write(1,*) 'Variables="W","T"'
write(1,'(A30)') 'zone t="Solução Analítica"'

do k=1,npast-1
T(k+1)=T(k)+dt
write(1,'(F20.4,F20.4)') -(-1 + exp(-T(k)/St)), T(k)
end do

write(1,'(A30)') 'zone t="Resultado Numérico"' 

! Calculando a velocidade das partículas (Utilizando Runge-Kutta de 4° ordem)

do k=1,npast-1


k1=dt*((-U(3))-1.0)/St
k2=dt*((-U(3)+0.5*k1)-1.0)/St
k3=dt*((-U(3)+0.5*k2)-1.0)/St
k4=dt*((-U(3)+k3)-1.0)/St

U(3)=U(3)+(1.0/6.0)*(k1+2.0*k2+2.0*k3+k4)

T(k+1)=T(k)+dt

write(1,'(F20.4,F20.4)') -U(3),T(k)


end do

end