program main
    implicit none

    integer:: op !Variable para leer la opcion

    print *, "Bienvenido a mi app!"
    do
        print *, "---------MENU---------"
        print *, "1. Comprar automovil"
        print *, "2. Modificar automovil"
        print *, "3. Vender automovil"
        print *, "4. Salir"
        print *, "Ingrese una opcion"
        read *,op !Leer la opcion del usuario
        op = op + 1
        select case (op)
            case(1)
               call comprarAuto()
            case (2)
                call modificarAuto()
            case (3)
                call venderAuto()
            case (4)
                print *, "Saliendo del sistema..."
                stop
            case default
                print *, "Opcion no valida, presione una tecla para continuar"
                read *
        end select
        call system("cls") !cls -> windows | clear -> linux LIMPIAR PANTALLA
    end do
end program main

subroutine comprarAuto()
    print *, "Vas a comprar un auto!"
    !Código necesario para comprar un auto
    print *, "Presione una tecla para continuar"
    read *
end subroutine comprarAuto 

subroutine venderAuto()
    print *, "Vas a vender un auto!"
    !Código necesario para vender un auto
    print *, "Presione una tecla para continuar"
    read *
end subroutine venderAuto

subroutine modificarAuto()
    print *, "Vas a modificar un auto!"
    !Código necesario para modificar un auto
    print *, "Presione una tecla para continuar"
    read *
end subroutine modificarAuto

