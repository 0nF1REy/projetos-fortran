      program grafico
      implicit none
      external f3
      real*8 x, xmin, xmax, delx, f3
      integer nx, i

      xmin = -2.5
      xmax = 2.5
      nx = 1000
      delx = (xmax - xmin)/dfloat(nx - 1)

      open (unit=1, file='dados_do_grafico.dat')
      do i = 1, nx
      x = xmin + dfloat(i-1)*delx

      write(*,*) ' '
      write(*,*) 'Meu intervalo nas abscissas é: ',delx
      write(*,*) 'x = ',x,'f3(x) = ',f3(x)
      write(1,*)x,f3(x)

      enddo
      close(1)

      end 

      real*8 function f3(x)
      implicit none
      real*8 x
      f3 = x**2
      return
      end