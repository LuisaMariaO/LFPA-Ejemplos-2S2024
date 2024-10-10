module analizadorSintacticoModule
    use tokenModule
    use errorModule
    implicit none

    type analizadorSintactico
        type(Token) :: tokens(10000)
        type(Error) :: errores(10000)

        integer :: iTokens
        integer :: iErrores
        integer :: analizandoToken


        contains
            procedure :: analizar
            procedure :: agregarError
            procedure :: controles
            procedure :: listaControles
            procedure :: listaControlesP
            procedure :: reconocerControl


            

    end type analizadorSintactico

    contains
    subroutine analizar(this, tokens, iTokens)
        !Estoy en el símbolo inicial
        class(analizadorSintactico), intent(inout) :: this
        type(Token), intent(in) :: tokens(10000)
        integer, intent(in) :: iTokens
        this%tokens = tokens
        this%iTokens = iTokens
        this%analizandoToken = 1


        call this%controles()
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

        
    end subroutine controles

    subroutine listaControles(this)
        class(analizadorSintactico), intent(inout) :: this
        call this%reconocerControl()
        
    end subroutine listaControles

    subroutine listaControlesP(this)
        class(analizadorSintactico), intent(inout) :: this
    end subroutine listaControlesP

    subroutine reconocerControl(this)
        class(analizadorSintactico), intent(inout) :: this
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

end module analizadorSintacticoModule