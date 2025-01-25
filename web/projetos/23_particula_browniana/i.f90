program particula_browniana

integer i,j,k               		! i,j,k = variáveis inteiras utilizadas em loops
real, allocatable :: X(:,:) 		! Matriz posição (x,y,z) das partículas
real, allocatable :: XINICIAL(:,:) 	! Matriz posição inicial (x,y,z) das partículas
real, allocatable :: U(:,:) 		! Matriz velocidade (x,y,z) da partícula
real, allocatable :: DES(:,:) 		! Matriz deslocamento quadrático de cada partícula
real, allocatable :: DESMEDIO(:) 	! Vetor deslocamento quadrático médio (para as 3 direções)
real, allocatable :: T(:)   		! Vetor tempo
real, allocatable :: f(:,:)   		! f = vetor randomico
real dt                 ! dt = passo de tempo
integer NREA            ! Número de realizações
real Pe 		        ! Número de Péclet
real frac		        ! Fração do intervalo total de tempo que será simulada a cada coleta de dados
real testereal		    ! Variável utilizada para fazer um teste 
integer testeinteiro 	! Variável utilizada para fazer um teste 

! Passo de tempo
dt=0.01

! Número de passos de tempo
npast=1000

! Número de realizações
NREA= 1000

! Pegando a informação referente ao valor do número de Péclet
write(*,*) 'Digite o numero de Peclet da partícula:'
read*,Pe

! Alocando variáveis na memória
allocate(X(NREA,3))
allocate(XINICIAL(NREA,3))
allocate(U(NREA,3))
allocate(T(npast+1))
allocate(f(NREA,3))
allocate(DES(NREA,3))
allocate(DESMEDIO(3))

! Distribuindo inicialmente as partículas
do i=1,NREA
 call randomica(0.0,100.0,X(i,1))
 call randomica(0.0,100.0,X(i,2))
 call randomica(0.0,100.0,X(i,3))
end do

! Anotando as posições iniciais de cada partícula em cada direção e em cada realização
do i=1,NREA
XINICIAL(i,1) = X(i,1)
XINICIAL(i,2) = X(i,2)
XINICIAL(i,3) = X(i,3)
end do

! Velocidade inicial das partículas
do i=1,NREA
U(i,1)=0.0
U(i,2)=0.0
U(i,3)=0.0
end do

U=0.0

! Abrindo um arquivo de saída para anotar os dados referentes ao deslocamento quadrático médio
open (2,file='deslocamento_quadradico_medio.plt')
write(2,*) 'Variables="DES1","DES2","DES3","T"'
write(2,'(A30)') 'zone t="Teoria do Einstein"'

do k=1,npast-1
T(k+1)=T(k)+dt
write(2,'(F20.4,F20.4,F20.4,F20.4)') 2.0*T(k),2.0*T(k),2.0*T(k), T(k)
end do

write(2,'(A30)') 'zone t="Nossas simulações"'

! Abrindo um arquivo de saída para anotar as posições ao longo do tempo para uma partícula teste arbitrária (trajetória)
open (1,file='trajetoria_tipica.plt')
write(1,*) 'Variables="X","Y","Z"'

! Abrindo um arquivo de saída para anotar as posições ao longo do tempo separadas por zonas para animar
open (3,file='animacao.plt')
write(3,*) 'Variables="X","Y","Z"'
write(3,*) 'zone t="1"'
write(3,'(F20.4,F20.4,F20.4)') X(1,1), X(1,2), X(1,3)

! Iniciando o processo de evolução temporal em diferentes intervalos de tempo
do k=1, npast

 ! Determinando a força Browniana atuante nesse time-step em todas as partículas
do i=1,NREA
 call randomica(-1.0,1.0,f(i,1))
 call randomica(-1.0,1.0,f(i,2))
 call randomica(-1.0,1.0,f(i,3))

! Normalizando essas forças para termos vetores unitários
f(i,1)= f(i,1)/sqrt(f(i,1)**2.0 + f(i,2)**2.0 + f(i,3)**2.0)
f(i,2)= f(i,2)/sqrt(f(i,1)**2.0 + f(i,2)**2.0 + f(i,3)**2.0)
f(i,3)= f(i,3)/sqrt(f(i,1)**2.0 + f(i,2)**2.0 + f(i,3)**2.0)

! Resolvendo a nova velocidade das partículas
U(i,1) = 1.0/Pe*((6.0/dt)**0.5)*f(i,1)
U(i,2) = 1.0/Pe*((6.0/dt)**0.5)*f(i,2)
U(i,3) = -1.0 +  1.0/Pe*((6.0/dt)**0.5)*f(i,3)

! Calculando a nova posição das partículas
X(i,1) = X(i,1) + U(i,1)*dt*Pe
X(i,2) = X(i,2) + U(i,2)*dt*Pe
X(i,3) = X(i,3) + U(i,3)*dt*Pe

end do

! Escrevendo as posições num arquivo de texto de uma partícula teste qualquer
write(1,'(F20.4,F20.4,F20.4)') X(1,1), X(1,2), X(1,3)

frac=k/npast

testereal=frac/0.05
testeinteiro=frac/0.05

if(testereal.eq.testeinteiro)then

write(3,*) 'zone t="',k,'"'
write(3,'(F20.4,F20.4,F20.4)') X(1,1), X(1,2), X(1,3)

! Calculando o deslocamento quadrático de cada partícula em cada direção
do i=1,NREA
DES(i,1)= (X(i,1) - XINICIAL(i,1))**2.0
DES(i,2)= (X(i,2) - XINICIAL(i,2))**2.0
DES(i,3)= (X(i,3) - XINICIAL(i,3))**2.0
end do

! Determinando o deslocamento quadrático fazendo uma média em cima das realizações
DESMEDIO(1)= SUM(DES(:,1))/NREA
DESMEDIO(2)= SUM(DES(:,2))/NREA
DESMEDIO(3)= SUM(DES(:,3))/NREA

! Anotando em um arquivo externo o valor do deslocamento quadrático médio e o intervalo de tempo no qual ele foi obtido
write(2,'(F20.4,F20.4,F20.4,F20.4)') DESMEDIO(1),DESMEDIO(2),DESMEDIO(3), k*dt

end if

end do

end

!************************************************************************************ Subrotinas Utilizadas no código **************************************************************************************!
subroutine randomica(a,b,c)
real a,b 	! a,b = range do numero randomico
real c      ! c = número randômico gerado

 call random_number(c)

 c = a + (b-a)*c

end subroutine randomica

!************************************************************************************************************************************************************************************************************!