module DeporteModule
    use EquipoModule
    implicit none

    type :: Deporte
        character(len=:), allocatable :: nombre
        type(Equipo) :: equipos(50)
        integer :: iEquipos = 1
    contains
        procedure :: crearDeporte
    end type Deporte

    contains
    subroutine crearDeporte (this, nombre)
        class(Deporte), intent(inout) :: this
        character(len=*), intent(in) :: nombre

        this%nombre = nombre
    end subroutine crearDeporte
end module DeporteModule