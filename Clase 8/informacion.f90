module informacionModule
    use analizadorModule
    use equipoModule
    use deporteModule
    implicit none

    type :: Informacion
       type(Token) :: tokens(200)
       character(len=:), allocatable :: nombreGrafica
       type(Deporte) :: deportes(100)

       integer :: iDeportes = 1
       contains
           procedure :: crearInformacion
           procedure :: iterarTokens
           procedure :: graficar
    end type Informacion

    contains
    subroutine crearInformacion(this, tokens)
        class(Informacion), intent(inout) :: this
        type(Token), intent(in) :: tokens(200)

        this%tokens = tokens
    end subroutine crearInformacion

    subroutine iterarTokens(this)
        class(Informacion), intent(inout) :: this
        integer :: i
        type(Deporte) :: deporte_actual
        type(Equipo) :: equipo_actual

        logical :: dentroDeporte = .false.
        logical :: dentroEquipo = .false.

        character(len=:), allocatable :: nombreEquipo, logo
        integer :: partidosJugados, porcentajeVictorias
        
        do i=1, 200
            if(this%tokens(i)%lexema=="grafica") then
                this%nombreGrafica = this%tokens(i+5)%lexema

            else if(this%tokens(i)%lexema=="deporte")then
                deporte_actual = Deporte(this%tokens(i+5)%lexema)
                dentroDeporte = .true.

            else if(this%tokens(i)%lexema=="equipo") then
                dentroEquipo = .true.
            else if(this%tokens(i)%lexema=="nombre" .and. dentroEquipo) then
                nombreEquipo = this%tokens(i+2)%lexema 
            else if(this%tokens(i)%lexema=="logo" .and. dentroEquipo) then
                logo = this%tokens(i+2)%lexema
            else if (this%tokens(i)%lexema=="porcentajeVictorias" .and. dentroEquipo) then
                porcentajeVictorias = this%tokens(i+2)%linea
            else if (this%tokens(i)%lexema=="partidosJugados" .and. dentroEquipo) then
                partidosJugados = this%tokens(i+2)%linea
            else if (this%tokens(i)%lexema=="}" .and. dentroEquipo) then
                equipo_actual = Equipo(nombreEquipo, logo, partidosJugados, porcentajeVictorias)
                dentroEquipo = .false.

                deporte_actual%equipos(deporte_actual%iEquipos) = equipo_actual
                deporte_actual%iEquipos = deporte_actual%iEquipos + 1

            else if (this%tokens(i)%lexema=="}" .and. dentroDeporte) then
                this%deportes(this%iDeportes) = deporte_actual
                this%iDeportes = this%iDeportes + 1
                dentroDeporte = .false.
            end if
        end do

        call this%graficar(this%deportes, this%nombreGrafica)
    end subroutine iterarTokens

    subroutine graficar(this, deportes, nombreGrafica)
        class (Informacion), intent(inout) :: this
        type(Deporte), intent(in) :: deportes(100)
        character(len=*), intent(in) :: nombreGrafica

        integer:: unit = 15
        integer :: i, j

        character(len=:), allocatable :: nombreSinComillas
        character(len=:), allocatable :: color
        

        integer :: promedio = 0
        integer:: sumaEquipos = 0
        integer:: numEquipos = 0

        !Abrir archivo para escribir el dot
        open(unit=unit, file="grafica.dot", status='replace', action='write')

        write(unit, '(A)') 'digraph G {'
        write(unit, '(A)') nombreGrafica // '[shape=Mdiamond]'

        !Iterar la informacion
        do i=1, this%iDeportes-1
            numEquipos = 0
            sumaEquipos = 0
            promedio = 0
            write(unit, '(A)') nombreGrafica // '->' // deportes(i)%nombre

            do j=1, deportes(i)%iEquipos-1
                numEquipos = numEquipos + 1
                sumaEquipos = sumaEquipos + deportes(i)%equipos(j)%porcentajeVictorias

                nombreSinComillas = deportes(i)%equipos(j)%nombre(2:len_trim(deportes(i)%equipos(j)%nombre)-1)
                if(deportes(i)%equipos(j)%porcentajeVictorias > 20) then
                    color = "green"
                else
                    color = "red"
                end if

                write(unit, '(A, A, A, A, I0, A, A, A, A)', advance="no") & 
                deportes(i)%equipos(j)%nombre , '[style=filled shape=box label="' , &
                nombreSinComillas , ' \n ' , &
                deportes(i)%equipos(j)%porcentajeVictorias , &
                '%"]', '[fillcolor=' , color , ']'

                write(unit, '(A)') deportes(i)%nombre 
                write(unit, '(A)') '->'
                write(unit, '(A)') deportes(i)%equipos(j)%nombre
            end do
            promedio = sumaEquipos / numEquipos
            nombreSinComillas = deportes(i)%nombre(2:len_trim(deportes(i)%nombre)-1)
            if(promedio > 20) then
                color = "green"
            else
                color = "red"
            end if
            write(unit, '(A, A, A, A, I0, A, A, A, A)', advance="no") & 
            deportes(i)%nombre , '[style=filled shape=box label="' , &
            nombreSinComillas , ' \n ' , &
            promedio , &
            '%"]', '[fillcolor=' , color , ']'
            
        end do


        write(unit, '(A)') '}'
        close(unit)

        !Convirtiendo el dot a png
        call system('dot -Tpng grafica.dot -o grafica.png')
    end subroutine graficar
end module informacionModule