program main 
    use analizadorLexicoModule
    use analizadorSintacticoModule
    implicit none
    character(len=:), allocatable :: entrada

    type(AnalizadorLexico) :: mi_analizadorLexico
    type(AnalizadorSintactico) :: mi_analizadorSintactico
    !RECIBIR ENTRADA DESDE PYTHON

    !Llamar al analizador léxico
    call mi_analizadorLexico%analizar(entrada)

    call mi_analizadorSintactico%analizar(mi_analizadorLexico%tokens, mi_analizadorLexico%iTokens)

    






end program main