module EquipoModule
    implicit none

    type :: Equipo
        character(len=:), allocatable :: nombre, logo
        integer :: partidosJugados, porcentajeVictorias
    contains
        procedure :: crearEquipo
        end type Equipo

    contains

    subroutine crearEquipo(this, nombre, logo, partidosJugados, porcentajeVictorias)
        class(Equipo), intent(inout) :: this
        character(len=*), intent(in) :: nombre, logo
        integer, intent(in) :: partidosJugados, porcentajeVictorias

        this%nombre = nombre
        this%logo = logo
        this%partidosJugados = partidosJugados
        this%porcentajeVictorias = porcentajeVictorias
    end subroutine crearEquipo
    
end module EquipoModule