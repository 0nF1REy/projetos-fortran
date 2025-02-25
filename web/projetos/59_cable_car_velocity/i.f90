program cable_car_velocity
    implicit none

    integer :: num_towers, i, total_dist, distance, nearest_tower
    real :: cable_length, velocity
    real, dimension(:), allocatable :: tower_positions
    logical :: valid_input

    write(*,*) "Simulação de Teleférico"
    write(*,*) "----------------------"

    cable_length = 0.0
    do while (cable_length <= 0.0)
        write(*,*) "Digite o comprimento do cabo (em pés):"
        read(*,*) cable_length
        if (cable_length <= 0.0) then
            write(*,*) "Erro: O comprimento do cabo deve ser positivo."
        end if
    end do

    num_towers = 0
    do while (num_towers <= 0)
        write(*,*) "Digite o número de torres (pelo menos 1):"
        read(*,*) num_towers
        if (num_towers <= 0) then
            write(*,*) "Erro: Deve haver pelo menos uma torre."
        end if
    end do

    allocate(tower_positions(num_towers))

    do i = 1, num_towers
        valid_input = .false.
        do while (.not. valid_input)
            write(*,*) "Digite a posição da torre ", i, " (em pés, entre 0 e ", cable_length, "):"
            read(*,*) tower_positions(i)
            if (tower_positions(i) >= 0.0 .and. tower_positions(i) <= cable_length) then
                valid_input = .true.
            else
                write(*,*) "Erro: Posição da torre inválida.  Deve estar entre 0 e ", cable_length, "."
            end if
        end do
    end do

    total_dist = 0

    write(*,*)
    write(*,*) "Relatório da Simulação do Teleférico"
    write(*,*) "Distância (ft)", "Torre Mais Próxima", "Velocidade (ft/sec)"
    write(*,*) "------------", "-----------------", "-----------------"

    do while (total_dist <= cable_length)

        call find_nearest_tower(total_dist, num_towers, tower_positions, nearest_tower, distance)

        velocity = calculate_velocity(distance)

        write(*,'(F12.2, I18, F20.2)') real(total_dist), nearest_tower, velocity

        total_dist = total_dist + 50
    end do

    deallocate(tower_positions)

    stop

contains

subroutine find_nearest_tower(total_dist, num_towers, tower_positions, nearest_tower, distance)
    integer, intent(in) :: total_dist, num_towers
    real, dimension(:), intent(in) :: tower_positions
    integer, intent(out) :: nearest_tower
    integer, intent(out) :: distance
    integer :: i
    real :: min_dist, current_dist

    min_dist = abs(real(total_dist) - tower_positions(1))
    nearest_tower = 1

    do i = 2, num_towers
        current_dist = abs(real(total_dist) - tower_positions(i))
        if (current_dist < min_dist) then
            min_dist = current_dist
            nearest_tower = i
        end if
    end do

    distance = int(min_dist)
end subroutine find_nearest_tower

    real function calculate_velocity(distance)
        integer, intent(in) :: distance

        if (distance > 10) then
            calculate_velocity = 4.25 + 0.0018 * distance
        else
            calculate_velocity = 6.25 + 1.2E-4 * distance - 0.0002 * real(distance) * real(distance)
        end if

    end function calculate_velocity

end program cable_car_velocity