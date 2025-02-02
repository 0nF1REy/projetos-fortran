module gnuplot_fortran
    implicit none
    contains
    subroutine plot2d(x,y,namef)
        CHARACTER (LEN=*) :: namef
        real, intent(in), dimension(:) :: x,y
        integer :: size_x, size_y, i
        size_x = size(x)
        size_y = size(y)
        if (size_x /= size_y) then
            print *, "Error: array size mismatch in plot2d subroutine"
            return
        else
            open(unit=10, file=namef, status='replace', action='write', iostat=i)
            if (i /= 0) then
              print *, "Error: Could not open file", namef
              return
            end if
            
            do i = 1, size(x)
                write(10,'(f8.3, 3x, f8.3)') x(i), y(i)
            end do
            close(10)
        end if
    end subroutine plot2d
end module gnuplot_fortran

! ****************************************************** 
module gnu_style
    implicit none
    contains
    subroutine create_png(frame_name,data_name)
        CHARACTER (LEN=*) :: frame_name
        CHARACTER (LEN=*) :: data_name
        integer :: i
        
        open (unit=20, file='frames/'//"temp.gnu", status='replace', action='write', iostat=i)
        if (i /= 0) then
          print *, "Error: Could not open file temp.gnu"
          return
        end if
        
        write(20,*) 'set terminal pngcairo transparent enhanced font "arial,10" fontscale 1.0 size 500, 300'
        write(20,*) "set output 'frames/"//frame_name//"'"
        write(20,*) "set xrange [0.0:6.28318531]"
        write(20,*) "set yrange [-1.0:1.0]"

        write(20,*) "plot 'data/"//data_name//"'"
        close(20)

        call execute_command('gnuplot frames/temp.gnu')

    end subroutine create_png

    subroutine execute_command(command)
        character(len=*), intent(in) :: command
        integer :: stat
        call system(command, stat)
        if (stat /= 0) then
          print *, "Error executing command: ", trim(command)
        end if
      end subroutine execute_command
end module gnu_style

! ****************************************************** 
program animation
    use gnuplot_fortran
    use gnu_style
    implicit none

    integer, parameter   :: n = 100
    real, parameter      :: pi = 4*atan(1.0)
    real, dimension(0:n) :: x, y
    real                 :: xmin = 0, xmax = 2*pi, dx

    character(12) :: data_name
    character(12) :: frame_name

    integer       :: i, frames = 250
    integer       :: status

    ! =================================================================
    ! TO PUT DATA FILES AND FRAMES IN CREATE DIRECTOR
    ! =================================================================
    call execute_command('mkdir data')
    call execute_command('mkdir frames')

    ! =================================================================
    ! GENERATE X AXIS
    ! =================================================================
    dx = (xmax - xmin)/n
    x(0:n) = [(i*dx, i=0, n)]

    ! =================================================================
    ! GENERATE FRAMES FOR ANIMATION 
    ! =================================================================
    do i = 1, frames

        ! =================================================================
        ! CREATE A FILENAME FOR DATA
        ! =================================================================   
        write(data_name, '(a, i4.4,a)') 'data', i, '.dat'
        
        ! =================================================================
        ! GENERATE Y AZIS FOR EACH FRAME
        ! =================================================================
        y = sin(0.01*i*x) / (x+1)
        
        ! =================================================================
        ! WRITE DATA FILES
        ! =================================================================
        call plot2d(x,y,'data/'//data_name)
        
        ! =================================================================
        ! CREATE A FILENAME FOR FRAME
        ! =================================================================
        write(frame_name,'(a, i4.4,a)') 'plot', i, '.png'

        ! =================================================================
        ! CREATE THE GNUPLOT SETTINGS
        ! =================================================================
        call create_png(frame_name,data_name)    
        print *, "Generating frame: ", i  
    end do

    ! =================================================================
    ! BUILD ANIMATION
    ! =================================================================
    call execute_command('ffmpeg -i frames/plot%04d.png animation.avi')

    ! =================================================================
    ! DELETE DATA FILES AND FRAMES
    ! =================================================================
    call execute_command('rm -rf data')
    call execute_command('rm -rf frames')

    print *, "Animation complete!"
    
end program animation