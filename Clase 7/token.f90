module tokenModule
    implicit none
    type :: Token
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: lexema
        integer :: linea, columna

        contains
        procedure :: crearToken
    end type Token

    contains
    subroutine crearToken(this, nombre, lexema, linea, columna) 
        class(Token), intent(inout) ::this
        character(len=*), intent(in)::nombre
        character(len=*), intent(in)::lexema
        integer, intent(in) :: linea
        integer, intent(in) :: columna

        this%nombre = nombre
        this%lexema = lexema
        this%linea = linea
        this%columna = columna
    end subroutine crearToken
end module tokenModule