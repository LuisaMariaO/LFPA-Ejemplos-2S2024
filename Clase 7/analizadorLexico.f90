module analizadorModule
    use tokenModule
    use errorModule
    implicit none

    type :: Analizador

        integer :: estado
        !Listas de tokens y de errores
        type(Token) :: tokens(200)
        type(Error) :: errores(200)

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
        procedure :: estado0
        procedure :: estado1
        procedure :: estado2
        procedure :: estado3
        procedure :: inicializarBuffer
        procedure :: imprimirTokens
        procedure :: imprimirErrores
        procedure :: generarReporteTokens
        procedure :: generarReporteErrores

    end type Analizador

    contains
        subroutine analizar(this, entrada)
            class (Analizador), intent(inout) :: this
            character(len=*), intent(in) :: entrada

            integer :: longitud !Longitud de la cadena de entrada
            this%i = 1 !Contador de caracteres

            !Volviendo 1 los contadores de tokens y errores
            this%iTokens = 1
            this%iErrores = 1


            !Obteniendo la longitud de la cadena de entrada
            longitud = len_trim(entrada)

            !Recorrer la entrada caracter por caracter

            do while(this%i <= longitud)
                select case(this%estado)
                    case(0)
                        call this%estado0(entrada(this%i:this%i))
                    case(1)
                        call this%estado1(entrada(this%i:this%i))
                    case(2)
                        call this%estado2(entrada(this%i:this%i))
                    case(3)
                        call this%estado3(entrada(this%i:this%i))
                end select
                this%i = this%i + 1
            end do
             !Imprimir los tokens
            call this%imprimirTokens()

            !Imprimir los errores
            !call this%imprimirErrores()

            !Generar reporte de tokens
            call this%generarReporteTokens('tokens.html')

            !Generar reporte de errores
            call this%generarReporteErrores('errores.html')
        end subroutine analizar

        subroutine inicializarEstado(this)
            class(Analizador), intent(inout) :: this
            this%estado = 0
        end subroutine inicializarEstado

        subroutine inicializarBuffer(this)
            class(Analizador), intent(inout) :: this
            if (.not. allocated(this%buffer)) allocate(character(len=0) :: this%buffer)
            this%buffer = ''
        end subroutine inicializarBuffer

        subroutine agregarToken(this, nombre, lexema, linea, columna)
            class(Analizador), intent(inout) :: this
            character(len=*), intent(in) :: nombre
            character(len=*), intent(in) :: lexema
            integer, intent(in) :: linea
            integer, intent(in) :: columna

            
            type(Token) :: token
            call token%crearToken(nombre, lexema, linea, columna)
            this%tokens(this%iTokens) = token
            this%iTokens = this%iTokens + 1
        end subroutine agregarToken

        subroutine agregarError(this, caracter, descripcion, linea, columna)
            class(Analizador), intent(inout) :: this
            character(len=*), intent(in) :: caracter
            character(len=*), intent(in) :: descripcion
            integer, intent(in) :: linea
            integer, intent(in) :: columna
            
            type(Error) :: error
            
            call error%crearError(caracter, descripcion, linea, columna)
            this%errores(this%iErrores) = error
            this%iErrores = this%iErrores + 1
        end subroutine agregarError

        subroutine imprimirTokens(this)
            class(Analizador), intent(inout) :: this
            integer :: i

            !print *, "Tokens encontrados:"

            do i = 1, this%iTokens - 1
                !print *, "Nombre: ", this%tokens(i)%nombre
                !print *, "Lexema: ", this%tokens(i)%lexema
                !print *, "Linea: ", this%tokens(i)%linea
                !print *, "Columna: ", this%tokens(i)%columna
            end do
        end subroutine imprimirTokens

        subroutine imprimirErrores(this)
            class(Analizador), intent(inout) :: this
            integer :: i

            !print *, "Errores encontrados:"

            do i = 1, this%iErrores - 1
                !print *, "Caracter: ", this%errores(i)%caracter
                !print *, "Descripcion: ", this%errores(i)%descripcion
                !print *, "Linea: ", this%errores(i)%linea
                !print *, "Columna: ", this%errores(i)%columna
            end do
        end subroutine imprimirErrores

        subroutine estado0(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            !print *, "Estado 0"

            
            if ((iachar(caracter) >= iachar('A') .and. iachar(caracter) <= iachar('Z')) .or. &
            (iachar(caracter) >= iachar('a') .and. iachar(caracter) <= iachar('z'))) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1

                !Transicion al estado 1
                this%estado = 1
            else if(caracter=='"') then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                !Transicion al estado 2
                this%estado = 2
            else if(iachar(caracter) >= iachar('0') .and. iachar(caracter) <= iachar('9')) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                !Transicion a estado 3
                this%estado = 3

            else if(caracter==':') then
                call this%agregarToken('dos_puntos',caracter,this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1
            else if(caracter=='{') then
                call this%agregarToken('llave_abre',caracter,this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1
            else if(caracter=='}') then
                call this%agregarToken('llave_cierra',caracter,this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1
            else if(caracter=='%') then
                call this%agregarToken('porcentaje',caracter,this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1
            else if(caracter==';') then
                call this%agregarToken('punto_y_coma',caracter,this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1
            else if(caracter== new_line('A')) then
                this%linea = this%linea + 1
                this%columna = 1
            else if(caracter==' ') then
                this%columna = this%columna + 1
            else
                !Agregar error
                call this%agregarError(caracter, 'Caracter no valido', this%linea, this%columna)
                this%columna = this%columna + 1
            end if

        end subroutine estado0

        subroutine estado1(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            character(len=:), allocatable :: tempBuffer  ! Variable temporal para almacenar el buffer
            !print *, "Estado 1"

            if ((iachar(caracter) >= iachar('A') .and. iachar(caracter) <= iachar('Z')) .or. &
            (iachar(caracter) >= iachar('a') .and. iachar(caracter) <= iachar('z'))) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1

            else
                !Almacenar el buffer en una variable temporal
                tempBuffer = trim(this%buffer)

                if (tempBuffer=="grafica") then
                    !Agregar token
                    call this%agregarToken("Palabra reservada [grafica]",tempBuffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer=="nombre") then
                    !Agregar token
                    call this%agregarToken("Palabra reservada [nombre]",tempBuffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer == 'deporte') then
                    call this%agregarToken('palabra_reservada [deporte]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer == 'equipo') then
                    call this%agregarToken('palabra_reservada [equipo]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer == 'partidosJugados') then
                    call this%agregarToken('palabra_reservada [partidosJugados]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer == 'porcentajeVictorias') then
                    call this%agregarToken('palabra_reservada [porcentajeVictorias]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else if (tempBuffer == 'logo') then
                    call this%agregarToken('palabra_reservada [logo]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    !this%columna = this%columna + 1
                    this%i=this%i-1
                    this%estado = 0
                else
                    call this%agregarError(tempBuffer, 'Palabra reservada no valida', this%linea, this%columna)
                    call this%inicializarBuffer()
                    this%columna = this%columna + 1
                    this%estado = 0
                end if
            end if

        end subroutine estado1

        subroutine estado2(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            !print *, "Estado 2"
            if(caracter=='"') then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                call this%agregarToken('cadena',this%buffer,this%linea, this%columna)
                call this%inicializarBuffer()
                this%estado = 0
            else
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
            end if

        end subroutine estado2

        subroutine estado3(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            !print *, "Estado 3"
            
            if(iachar(caracter) >= iachar('0') .and. iachar(caracter) <= iachar('9')) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
            else

                call this%agregarToken('entero',trim(this%buffer),this%linea, this%columna)
                call this%inicializarBuffer()
                !this%columna = this%columna + 1
                this%estado = 0
                this%i = this%i - 1
            end if

        end subroutine estado3

        subroutine generarReporteTokens(this, archivo)
            class(Analizador), intent(in) :: this
            character(len=*), intent(in) :: archivo
            integer :: i
            integer :: unit = 12
        
            ! Abre el archivo para escritura
            open(unit=unit, file=archivo, status='replace', action='write')
        
            ! Escribe el encabezado de la tabla
            write(unit, '(A)') "<html><body><table border='1'>"
            !Agregar en la etiqueta <head> Estilos css deseados
            write(unit, '(A)') "<tr><th>Nombre</th><th>Lexema</th><th>Linea</th><th>Columna</th></tr>"
        
            ! Llena la tabla con los datos de los tokens
            do i = 1, this%iTokens-1
                write(unit, '(A, A, A, A, A, I0, A, I0, A)', advance="no") &
                    "<tr><td>", trim(this%tokens(i)%nombre), "</td><td>", &
                    trim(this%tokens(i)%lexema), "</td><td>", &
                    this%tokens(i)%linea, "</td><td>", &
                    this%tokens(i)%columna, "</td></tr>"
            end do
        
            ! Cierra la tabla y el HTML
            write(unit, '(A)') "</table></body></html>"
        
            ! Cierra el archivo
            close(unit)
        end subroutine generarReporteTokens
        
    
        subroutine generarReporteErrores(this, archivo)
            class(Analizador), intent(in) :: this
            character(len=*), intent(in) :: archivo
            integer :: i
            integer :: unit =11
        
            ! Abre el archivo para escritura
            open(unit=unit, file=archivo, status='replace', action='write')
        
            ! Escribe el encabezado de la tabla
            write(unit, '(A)') "<html><body><table border='1'>"
            write(unit, '(A)') "<tr><th>Caracter</th><th>Descripcion</th><th>Linea</th><th>Columna</th></tr>"
        
            ! Llena la tabla con los datos de los errores
            do i = 1, this%iErrores-1
                write(unit, '(A, A, A, A, A, I0, A, I0, A)', advance="no") &
                    "<tr><td>", trim(this%errores(i)%caracter), "</td><td>", &
                    trim(this%errores(i)%descripcion), "</td><td>", &
                    this%errores(i)%linea, "</td><td>", &
                    this%errores(i)%columna, "</td></tr>"
            end do
        
            ! Cierra la tabla y el HTML
            write(unit, '(A)') "</table></body></html>"
        
            ! Cierra el archivo
            close(unit)
        end subroutine generarReporteErrores
end module analizadorModule

!Las veces que se resta a i (this%i = this%i - 1) es para que no se pierdan tokens cuando estos vienen sin un espacio entre ellos