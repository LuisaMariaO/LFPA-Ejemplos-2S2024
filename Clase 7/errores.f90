module errorModule
    implicit none
    type :: Error
        character(len=:), allocatable :: caracter
        character(len=:), allocatable :: descripcion
        integer :: linea, columna

        contains
        procedure :: crearError
    end type Error

    contains
    subroutine crearError(this, caracter, descripcion, linea, columna) 
        class(Error), intent(inout) ::this
        character(len=*), intent(in)::caracter
        character(len=*), intent(in)::descripcion
        integer, intent(in) :: linea
        integer, intent(in) :: columna

        this%caracter = caracter
        this%descripcion = descripcion
        this%linea = linea
        this%columna = columna
    end subroutine crearError
end module errorModule