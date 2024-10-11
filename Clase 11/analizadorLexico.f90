module analizadorLexicoModule
    use tokenModule
    use errorModule

    implicit none

    type:: AnalizadorLexico

    integer :: estado
        !Listas de tokens y de errores
        type(Token) :: tokens(10000)
        type(Error) :: errores(10000)

        !Linea y columna actuales
        integer :: linea = 1
        integer :: columna = 1

        !indices de las listas de tokens y errores
        integer :: iTokens = 1
        integer :: iErrores = 1

        !Indice para el control de la cadena de entrada
        integer :: i = 1

        character(len=:), allocatable :: buffer !Buffer de lectura
        contains
        procedure :: analizar
        procedure :: inicializarEstado
        procedure :: agregarToken
        procedure :: agregarError

    end type AnalizadorLexico

    contains

    subroutine inicializarEstado(this)
        class(AnalizadorLexico), intent(inout) :: this
        this%estado = 0
    end subroutine inicializarEstado

    subroutine analizar(this, entrada)
        class(AnalizadorLexico), intent(inout) :: this
        character(len=*), intent(in) :: entrada

        this%iTokens = 1
        this%iErrores = 1

        call this%agregarToken("menor_que", "<")
        call this%agregarToken("signoAdimacion", "!")
        call this%agregarToken("guion", "-")
        call this%agregarToken("guion", "-")
        call this%agregarToken("pr_controles", "Controles")
        call this%agregarToken("pr_contenedor", "Contenedor")
        call this%agregarToken("identificador", "contlogin")
        call this%agregarToken("puntoYComa", ";")
        call this%agregarToken("pr_contenedor", "Contenedor")
        call this%agregarToken("identificador", "contFondo")
        call this%agregarToken("puntoYComa", ";")
        call this%agregarToken("pr_controles", "Controles")
        call this%agregarToken("guion", "-")
        call this%agregarToken("guion", "-")
        call this%agregarToken("mayor_que", ">")

        call this%agregarToken("menor_que", "<")
        call this%agregarToken("signoAdimacion", "!")
        call this%agregarToken("guion", "-")
        call this%agregarToken("guion", "-")
        call this%agregarToken("pr_propiedades", "propiedades")
        call this%agregarToken("identificador", "contlogin")
        call this%agregarToken("punto", ".")
        call this%agregarToken("pr_propiedad", "setAncho")
        call this%agregarToken("parentesis_abierto", "(")
        call this%agregarToken("numero", "100")
        call this%agregarToken("parentesis_cerrado", ")")
        call this%agregarToken("puntoYComa", ";")
        call this%agregarToken("pr_propiedades", "propiedades")
        call this%agregarToken("guion", "-")
        call this%agregarToken("guion", "-")
        call this%agregarToken("mayor_que", ">")


    end subroutine analizar

    subroutine agregarToken(this, nombre, lexema)
        class(AnalizadorLexico), intent(inout) :: this
        character(len=*), intent(in) :: nombre
        character(len=*), intent(in) :: lexema
        call crearToken(this%tokens(this%iTokens), nombre, lexema, this%linea, this%columna)
        this%iTokens = this%iTokens + 1
    end subroutine agregarToken

    subroutine agregarError(this, caracter, descripcion)
        class(AnalizadorLexico), intent(inout) :: this
        character(len=*), intent(in) :: caracter
        character(len=*), intent(in) :: descripcion
        call crearError(this%errores(this%iErrores), caracter, descripcion, this%linea, this%columna)
        this%iErrores = this%iErrores + 1
    end subroutine agregarError


end module analizadorLexicoModule