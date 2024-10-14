module analizadorSintacticoModule
    use tokenModule
    use errorModule
    use controlModule
    implicit none

    type analizadorSintactico
        type(Token) :: tokens(10000)
        type(Error) :: errores(10000)
        type(Control) :: mis_controles(100)

        integer :: iTokens
        integer :: iErrores
        integer :: analizandoToken
        integer :: iControles


        contains
            procedure :: analizar
            procedure :: agregarError
            procedure :: controles
            procedure :: listaControles
            procedure :: listaControlesP
            procedure :: reconocerControl
            procedure :: agregarControl
            procedure :: PROPIEDADES
            procedure :: LISTA_PROPIEDADES
            procedure :: LISTA_PROPIEDADES_P
            procedure :: PROPIEDAD
            procedure :: LISTA_VALORES
            procedure :: LISTA_VALORES_P
            procedure :: VALOR
            procedure :: agregarPropiedad
            procedure :: traducir

            

    end type analizadorSintactico

    contains
    subroutine analizar(this, tokens, iTokens)
        !Estoy en el símbolo inicial
        class(analizadorSintactico), intent(inout) :: this
        type(Token), intent(in) :: tokens(10000)
        integer, intent(in) :: iTokens
        integer :: i
        type(Control) :: controlInicial
        !Para el control inicial
        character(len=:), allocatable :: tipo
        character(len=:), allocatable :: id


        this%tokens = tokens
        this%iTokens = iTokens
        this%analizandoToken = 1
        this%iControles = 1

        tipo="this"
        id="this"
        

        !CREACION DE EL CONTROLADOR PRINCIPAL QUE CONTIENE LA BASE HTML
        call this%agregarControl(tipo, id)
        !Obtengo el controloador que acabo de crear y le asigno el html inicial y final
        controlInicial%html_apertura = controlInicial%html_apertura // '<html><head><link href="estilos.css" rel="stylesheet"type="text/css" /></head><body>'
        controlInicial%html_cierre = controlInicial%html_cierre // '</body></html>'

        call this%controles()
        call this%PROPIEDADES()
        call this%traducir()

        print*, ""
        do i = 1, this%iControles - 1
            print*, "Control: ", this%mis_controles(i)%tipo
            print*, "Identificador: ", this%mis_controles(i)%identificador
            print*, "Ancho: ", this%mis_controles(i)%ancho
        end do

    end subroutine analizar

    subroutine controles(this)
        class(analizadorSintactico), intent(inout) :: this

        if(this%tokens(this%analizandoToken)%lexema == "<") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [<]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "!") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [!]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "Controles") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Controles]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        call this%listaControles()

        if(this%tokens(this%analizandoToken)%lexema == "Controles") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Controles]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == ">") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [>]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        
    end subroutine controles

    subroutine listaControles(this)
        class(analizadorSintactico), intent(inout) :: this
        call this%reconocerControl()
        call this%listaControlesP()
        
    end subroutine listaControles

    recursive subroutine listaControlesP(this)
        class(analizadorSintactico), intent(inout) :: this

        call this%reconocerControl()
        if(this%tokens(this%analizandoToken)%nombre .NE. "pr_controles") then
            call this%listaControlesP()
        else
            print *, "Fin de la lista de controles"	
        end if
    end subroutine listaControlesP

    subroutine reconocerControl(this)
        class(analizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: tipo
        character(len=:), allocatable :: id

        if(this%tokens(this%analizandoToken)%nombre == "pr_contenedor")then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            tipo = "contenedor"
            this%analizandoToken = this%analizandoToken + 1
            !TRABAJAR VARIOS ELSE PARA RECONOCER ETIQUETA, TEXTO, BOTÓN, ETC....
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [tipo de Contenedor]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%nombre == "identificador") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            id = this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1

        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [identificador]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%nombre == "puntoYComa") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [;]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        call this%agregarControl(tipo, id)
    end subroutine reconocerControl

    subroutine agregarError(this, caracter, descripcion, linea, columna)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=*), intent(in) :: caracter
        character(len=*), intent(in) :: descripcion
        integer, intent(in) :: linea
        integer, intent(in) :: columna
        
        type(Error) :: error
        
        call error%crearError(caracter, descripcion, linea, columna)
        this%errores(this%iErrores) = error
        this%iErrores = this%iErrores + 1
    end subroutine agregarError

    subroutine agregarControl(this, tipo, id)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: tipo
        character(len=:), allocatable :: id
        type(Control) :: control

        call control%crearControl(tipo, id)
        this%mis_controles(this%iControles) = control
        this%iControles = this%iControles + 1

    end subroutine agregarControl

    subroutine PROPIEDADES(this)
        class(AnalizadorSintactico), intent(inout) :: this

        if(this%tokens(this%analizandoToken)%lexema == "<") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [<]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "!") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [!]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "propiedades") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Controles]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        call this%LISTA_PROPIEDADES()

        if(this%tokens(this%analizandoToken)%lexema == "propiedades") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Controles]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "-") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [-]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == ">") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [>]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        
    end subroutine PROPIEDADES

    subroutine LISTA_PROPIEDADES(this)
        class(AnalizadorSintactico), intent(inout) :: this
        call this%PROPIEDAD()
        call this%LISTA_PROPIEDADES_P()
    end subroutine LISTA_PROPIEDADES

    subroutine LISTA_PROPIEDADES_P(this)
        class(AnalizadorSintactico), intent(inout) :: this
        
    end subroutine LISTA_PROPIEDADES_P

    subroutine PROPIEDAD(this)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: lista_valores !String
        character(len=:), allocatable :: id
        character(len=:), allocatable :: tipo_propiedad

        lista_valores = ""
        if(this%tokens(this%analizandoToken)%nombre == "identificador") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            id = this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [identificador]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == ".") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
            
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [.] ", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "setAncho") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            tipo_propiedad = "ancho"
            this%analizandoToken = this%analizandoToken + 1
            !TRABAJAR VARIOS ELSE PARA RECONOCER setalto, setancho, setcolorletra, etc....
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Palabra reservada de propiedad]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        if(this%tokens(this%analizandoToken)%lexema == "(") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1

        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [Palabra reservada de propiedad]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if
       
        call this%LISTA_VALORES(lista_valores)

        if(this%tokens(this%analizandoToken)%lexema == ")") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [)]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        
        if(this%tokens(this%analizandoToken)%lexema == ";") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [;]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        !Guardar la propiedad en el control correspondiente
        call this%agregarPropiedad(id, tipo_propiedad, lista_valores)
    end subroutine PROPIEDAD

    subroutine LISTA_VALORES(this, lista_valores_string)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: lista_valores_string

        call this%valor(lista_valores_string)
        call this%LISTA_VALORES_P(lista_valores_string)
        
    end subroutine LISTA_VALORES

    recursive subroutine LISTA_VALORES_P(this, lista_valores_string)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: lista_valores_string

        if(this%tokens(this%analizandoToken)%lexema == ",") then
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            lista_valores_string = lista_valores_string // this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [,]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if

        call this%VALOR(lista_valores_string)

        if(this%tokens(this%analizandoToken)%nombre .NE. "coma") then
            call this%LISTA_VALORES_P(lista_valores_string)
        
        else
            print * , "Fin de la lista de valores"
        end if

    end subroutine LISTA_VALORES_P

    subroutine VALOR(this , lista_valores_string)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: lista_valores_string
        
        if(this%tokens(this%analizandoToken)%nombre == "cadena" .or. this%tokens(this%analizandoToken)%nombre == "numero" .or. this%tokens(this%analizandoToken)%nombre == "identificador") then !LOS TIPOS DEFINIDOS EN SU GRAMATICA
            lista_valores_string = lista_valores_string // this%tokens(this%analizandoToken)%lexema
            print *, "VALOR ", lista_valores_string
            print*, "Analizando token: ", this%tokens(this%analizandoToken)%lexema
            this%analizandoToken = this%analizandoToken + 1
        else
            call this%agregarError(this%tokens(this%analizandoToken)%lexema,"ERROR SINTACTICO: Se esperaba [cadena o entero o identificador]", this%tokens(this%analizandoToken)%linea, this%tokens(this%analizandoToken)%columna)
            return
        end if
    end subroutine VALOR

    subroutine agregarPropiedad(this, id, tipo_propiedad, lista_valores_string)
        class(AnalizadorSintactico), intent(inout) :: this
        character(len=:), allocatable :: id
        character(len=:), allocatable :: tipo_propiedad
        character(len=:), allocatable :: lista_valores_string
        integer :: i

        !Guardar la propiedad en el control correspondiente
        do i = 1,this%iControles
            if(this%mis_controles(i)%identificador == id) then
                print *, "Encontrado"
                if(tipo_propiedad == "ancho") then
                    this%mis_controles(i)%ancho = lista_valores_string
                end if
                !TRABAJAR VARIOS ELSE PARA AGREGAR SEGÚN EL TIPO DE PROPIEDAD (COLOR, TEXTO, ETC...)
            exit
            end if
        end do
    end subroutine agregarPropiedad

    subroutine traducir(this)
        class(AnalizadorSintactico), intent(inout) :: this
        integer :: i

        do i = 1, this%iControles - 1
           
        end do

    end subroutine traducir


end module analizadorSintacticoModule