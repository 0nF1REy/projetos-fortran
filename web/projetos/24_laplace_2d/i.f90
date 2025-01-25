program laplace_2d

integer n,m,i,j, k
real, dimension(:), allocatable:: T
real, dimension(:,:), allocatable:: A
real, dimension(:), allocatable:: B
real, dimension(:), allocatable:: x
real, dimension(:), allocatable:: y
real xmin,xmax,ymin,ymax
real deltax,deltay
real C1,C2,C3
real Tsup, Tinf, Tesq, Tdir

!Lista de variáveis
! n - número de nós em y
! m - número de nós em x
! i,j, k - variáveis inteiras utilizadas em loops
! T - vetor temperatura
! A - matriz dos coeficientes
! B - vetor de termos fontes e condições de contorno
! x - vetor posição horizontal x
! y - vetor posição vertical y
! xmin, xmax, ymin, ymax - coordenadas dos nós de quina da malha
! deltax, deltay - espaçamento entre nós em x e y respectivamente 
! C1, C2, C3 - constantes associadas à montagem da matriz dos coeficientes
! Tsup, Tinf, Tesq, Tdir - temperaturas dos contornos

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Lendo informações elementares para a confecção da malha e para a formulação do problema

! Número de nós em x e y
n=30
m=30

! Coordenadas dos vértices da malha
xmin=0.0
xmax=1.0
ymin=0.0
ymax=1.0

! Alocando variáveis
allocate(T(m*n))
allocate(A(m*n,m*n))
allocate(B(m*n))
allocate(x(n))
allocate(y(m))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Criando a malha
deltax=(xmax-xmin)/(n-1)

deltay=(ymax-ymin)/(m-1)

x(1)=xmin
do i=2,n
x(i)=x(i-1)+deltax
end do

y(1)=ymin
do i=2,m
y(i)=y(i-1)+deltay
end do

! Determinando os coeficientes C1, C2 e C3
 C1 = 2.0/(deltax**2.0) + 2.0/(deltay**2.0)
 C2 = 1.0/(deltax**2.0)
 C3 = 1.0/(deltay**2.0)


! Condições iniciais
T=100.0
A=0.0
B=0.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Obtendo as informações referentes ao contorno
write(*,*) 'Digite a temperatura da parede inferior:'
read*,Tinf

write(*,*) 'Digite a temperatura da parede superior:'
read*,Tsup

write(*,*) 'Digite a temperatura da lateral esquerda:'
read*,Tesq

write(*,*) 'Digite a temperatura da lateral direita:'
read*,Tdir

! Montando a matriz dos coeficientes e o vetor de termos fontes e condições de contorno...

! CONTORNO

! Parede inferior
do j=1,n
i=j
A(i,i) = 1.0
B(i) = Tinf
end do

! Lateral esquerda 
do j=1,m-2
i=j*n + 1
A(i,i) = 1.0
B(i) = Tesq
end do

! Lateral direita 
do j=1,m-2
i=(j+1)*n 
A(i,i) = 1.0
B(i) = Tdir
end do

! Parede superior 
do j=1,n
i=(n*(m-1)) + j
A(i,i) = 1.0
B(i) = Tsup
end do

!**************************************************************************!

! Resolvendo o problema em regime permanente
  do k=1,m-2
   do j=1,n-2
	i=(n*k) + 1 + j
	A(i,i) = 1.0
	A(i,i+1) = -C2/C1
	A(i,i-1) = -C2/C1
	A(i,i+n) = -C3/C1
	A(i,i-n) = -C3/C1
	B(i) =  0.0
   end do
   end do


! Resolvendo o sistema linear
 call jacobi(A,T,B,n*m,500)


! Escrevendo o arquivo de saída
open (1,file='saida_permanente.plt')
write(1,*) 'Variables="x","y","T"'
write(1,*) 'ZONE F=POINT,I='
write(1,*) n
write(1,*) ',J='
write(1,*) m
do i=1,n
do j=1,m
write(1,'(F12.4,F12.4,F12.4)')x(i),y(j),T(i+((j-1)*n))
end do
end do




end

! Subrotina para solução do sistema linear
subroutine jacobi(A,x,b,n,k)

implicit none
integer i,j,n,iter,k
real A(n,n), C(n,n), h(n), l(n)
real x(n),b(n),f(n),g

iter=0

do i=1,n
x(i)=1.0
end do

100 do j=1,n
	do i=1,n
	if(j.ne.i) then
	f(i)=(-A(i,j)*x(j))
	end if
	end do
	end do

	g=sum(f)

	do i=1,n
	x(i)=(b(i)+g)/A(i,i)
	end do

	iter=iter+1
	
	do i=1,n
	f(i)=0
	end do

	g=0

	if(iter.ne.k) then
	go to 100
	end if

	iter=0

	do i=1,n
	h(i)=b(i)/A(i,i)
	C(i,i)=0
	end do

	do i=1,n
	do j=1,n
	if(i.ne.j) then
	C(i,j)=-A(i,j)/A(i,i)
	end if
	end do
	end do

200	l=matmul(C,x)

	do i=1,n
	x(i)=l(i)+h(i)
	end do

	iter=iter+1

	if(iter.ne.k) then
	go to 200
	end if

end