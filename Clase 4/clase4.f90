module AutoModule
    implicit none

    type :: Auto
    character(len=256) :: marca
    character(len=256) :: color
    integer :: gasolina
    contains
      procedure :: inicializar
      !procedure :: mostrar
      procedure :: agregarGasolina
      procedure :: quitarGasolina
    end type Auto

    contains
    !Inicializar autos
    subroutine inicializar(this, marca, color, gasolina)
        class(Auto), intent(inout) :: this
        character(len=256), intent(in) :: marca
        character(len=256), intent(in) :: color
        integer, intent(in) :: gasolina

        this%marca = marca
        this%color = color
        this%gasolina = gasolina
    end subroutine inicializar

    !Agregar gasolina
    subroutine agregarGasolina(this, gasolina)
        class(Auto), intent(inout) :: this
        integer, intent(in) :: gasolina

        this%gasolina = this%gasolina + gasolina
    end subroutine

    !Quitar gasolina
    subroutine quitarGasolina(this, gasolina)
        class(Auto), intent(inout) :: this
        integer, intent(in) :: gasolina

        integer :: resultante
        resultante = this%gasolina - gasolina
        
        if(resultante>=0)then
            this%gasolina = this%gasolina - gasolina
        else
            print *, "No se puede quitar esa cantidad de gasolina, gasolina negativa"
        endif

        
    end subroutine


end module AutoModule

module global_vars
    use AutoModule
    integer :: n=1
    integer :: i
    type(Auto), dimension(100) :: inventario
end module global_vars

program main
    use AutoModule
    use global_vars
    implicit none
    integer:: op !Variable para leer la opcion

    print *, "Bienvenido a mi app!"
    do
        print *, "---------MENU---------"
        print *, "1. Crear automovil"
        print *, "2. Acciones de automovil"
        print *, "3. Crear informe"
        print *, "4. Salir"
        print *, "Ingrese una opcion"
        read *,op !Leer la opcion del usuario
      
        select case (op)
            case(1)
               call crearCarroArchivo()
            case (2)
                call accionesArchivo()
            case (3)
                call crearInforme()
            case (4)
                print *, "Saliendo del sistema..."
                do i=1, n-1
                    print *, "Auto ", i
                    print *, "Marca: ", inventario(i)%marca
                    print *, "Color: ", inventario(i)%color
                    print *, "Gasolina: ", inventario(i)%gasolina
                end do
                stop
            case default
                print *, "Opcion no valida, presione una tecla para continuar"
                read *
        end select
        call system("cls") !cls -> windows | clear -> linux LIMPIAR PANTALLA
    end do
end program main

subroutine crearCarroArchivo()
    integer :: iunit, ios, pos, gasolina_int
    character(len=256) :: marca, color, gasolina, linea, comando

    !Aasignar unidad al archivp
    iunit = 10

    open(unit=iunit, file="entrada.inv", status="old", action="read", iostat=ios)

    if(ios /= 0) then
        print *, "Error al abrir el archivo"
        stop
    endif

    do
        read(iunit, '(A)', iostat=ios)linea
        if(ios /= 0) exit
        linea = trim(linea)

        
        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, ' ')
        if(pos > 0)then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            !Separar por ;
            pos = index(linea, ';')
            if (pos > 0) then
                marca = linea(1:pos-1)
                linea = trim(linea(pos+1:))

                !Siguiente atributo
                pos = index(linea, ';')
                if (pos > 0) then
                    color = linea(1:pos-1)

                    !Pasando de cadena a int
                    gasolina = trim(linea(pos+1:))
                    read(gasolina, '(I10)', iostat=ios) gasolina_int !O F10.2 -> real

                    if (comando=="crear_carro")then
                        call crearCarro(marca, color, gasolina_int)
                    endif
        
                endif
            endif
            endif
        
    end do
    close(unit=iunit)
      read *
end subroutine crearCarroArchivo

subroutine crearCarro(marca, color, gasolina)
    use AutoModule
    use global_vars
    !dummy auguments
    character(len=256), intent (in) :: marca
    character(len=256), intent (in) :: color
    integer, intent (in) :: gasolina
    
    type(Auto) :: nuevoAuto
    call nuevoAuto%inicializar(marca, color, gasolina)
    inventario(n) = nuevoAuto
    n = n + 1

   
    read *
end subroutine crearCarro

subroutine accionesArchivo()
    use AutoModule
    use global_vars
    integer :: iunit, ios, pos, gasolina_int
    character(len=256) :: linea, comando, marca, color, gasolina


    iunit = 11

    ! Abre el archivo en modo lectura
    open(unit=iunit, file="acciones.mov", status="old", action="read", iostat=ios)

    ! Verifica si hubo un error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo."
        stop
    endif

     ! Lee e imprime el archivo línea por línea
    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
       
        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, ' ')
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))
            
            ! Separar por ';'
            pos = index(linea, ';')
            if (pos > 0) then
                marca = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                
                pos = index(linea, ';')
                if (pos > 0) then
                    color = linea(1:pos-1)
                    !Pasando de cadena a int
                    gasolina = trim(linea(pos+1:))
                    read(gasolina, '(I10)', iostat=ios) gasolina_int
                    

                    if (comando=="agregar_gasolina")then
                        call agregar_gasolina(marca, color, gasolina_int)
                    else if(comando=="quitar_gasolina")then
                        call quitar_gasolina(marca, color, gasolina_int)
                        endif
                    endif
                endif
            endif
    end do
    close(unit=iunit)
    read *
end subroutine accionesArchivo
    
    
subroutine agregar_gasolina(marca, color, gasolina)
    use AutoModule
    use global_vars
     ! dummy arguments      
    character(len=256), intent (in) :: marca
    character(len=256), intent (in) :: color
    integer, intent (in) :: gasolina

    do i=1, n-1
        if(inventario(i)%marca == marca .and. inventario(i)%color == color)then
            call inventario(i)%agregarGasolina(gasolina)
        endif
    end do
end subroutine agregar_gasolina

subroutine quitar_gasolina(marca, color, gasolina)
    use AutoModule
    use global_vars
     ! dummy arguments      
    character(len=256), intent (in) :: marca
    character(len=256), intent (in) :: color
    integer, intent (in) :: gasolina

    do i=1, n-1
        if(inventario(i)%marca == marca .and. inventario(i)%color == color)then
            call inventario(i)%quitarGasolina(gasolina)
        endif
    end do
end subroutine
subroutine crearInforme()
    use AutoModule
    use global_vars

    integer iunit, ios

    iunit = 20

    !Replace crea el archivo si no existe y reemplaza su contenido si ya existe
    open(unit=iunit, file="informe.txt", status="replace", action="write", iostat=ios)

    !Revisar si el archivo se abrió correctamente
    if(ios /= 0) then
        print *, "Error al abrir el archivo"
        stop  
    endif

    !Encabezado del informe
    ! A -> Alfanumérico
    !I -> Números enteros
    !F -> Números flotantes
    write(iunit, '(A30, A30, A30)')"Marca", "Color", "Gasolina"

    do i=1, n-1
        write(iunit, '(A30, A30, I30)') trim(inventario(i)%marca), trim(inventario(i)%color), inventario(i)%gasolina
    end do

    close(unit=iunit)
    print *, "Informe creado con exito"
    print *, "Presione una tecla para continuar"
    read *

end subroutine crearInforme

