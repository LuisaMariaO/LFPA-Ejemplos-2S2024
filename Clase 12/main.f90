program main

    implicit none

    type :: error
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
        character(len=100) :: token
        character(len=100) :: descripcion
    end type error

    !OBTENER LAS LISTAS DE ERRORES DEL ANALIZADOR LÉXICO Y SINTÁCTICO
    !mi_analizadorLexico%errores y mi_analizadorSintactico%errores
    type(error) :: errores(100)

    character(len=:), allocatable :: buffer
    character(len=:), allocatable :: contenido
    integer :: ios
    integer :: i
    integer :: num_errores
    !mi_analizadorLexico%iErrores

    contenido = ''

    num_errores = 0

    errores(1)%tipo = 'Sintactico'
    errores(1)%linea = 1
    errores(1)%columna = 1
    errores(1)%token = 'pr_controles'
    errores(1)%descripcion = 'Se esperaba[;]'

    num_errores = num_errores +1

    errores(2)%tipo = 'Lexico'
    errores(2)%linea = 2
    errores(2)%columna = 2
    errores(2)%token = ''
    errores(2)%descripcion = 'Caracter [#] no reconocido'
    num_errores = num_errores +1

    !Leyendo la entrada de python
    do
        read(*, '(A)', IOSTAT = ios) buffer
        if(ios /= 0) exit

        contenido = trim(contenido) // trim(buffer) // new_line('a')
    end do

    !Imprimimos los errores tanto léxicos como sintácticos
    do i = 1, num_errores
        print '(A, A, I10, A, I10, A, A, A, A)', trim(errores(i)%tipo), "|", errores(i)%linea, "|", errores(i)%columna, "|", trim(errores(i)%token), "|", trim(errores(i)%descripcion)

    end do

    !Iterar la otra lista de errores
end program main