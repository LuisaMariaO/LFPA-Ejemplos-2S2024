module ControlModule
    implicit none
        type :: Control
        character(len=:), allocatable :: tipo
        character(len=:), allocatable :: identificador
        character(len=:), allocatable :: texto
        character(len=:), allocatable :: alineacion
        character(len=:), allocatable :: marcado
        character(len=:), allocatable :: grupo
        character(len=:), allocatable :: colorLetra
        character(len=:), allocatable :: colorFondo
        character(len=:), allocatable :: ancho
        character(len=:), allocatable :: alto
        character(len=:), allocatable :: html_apertura
        character(len=:), allocatable :: html_cierre

        contains
        procedure :: crearControl
        end type Control
    contains

        subroutine crearControl(this, tipo, identificador)
            class(Control), intent(inout) :: this
            character(len=*), intent(in) :: tipo
            character(len=*), intent(in) :: identificador

            this%tipo = tipo
            this%identificador = identificador
        end subroutine crearControl
        
    
end module ControlModule