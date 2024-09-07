program main
    use analizadorModule
    implicit none
    character(len=:), allocatable :: entrada
    type (Analizador) :: mi_analizador

    entrada = "grafica: {" // new_line('A') // &
    "nombre: ""Grafico de Rendimiento Deportivo"";" // new_line('A') // &
    "    deporte: {" // new_line('A') // &
    "        nombre: ""Futbol"";" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""FC Barcelona"";" // new_line('A') // &
    "            partidosJugados: 38;" // new_line('A') // &
    "            porcentajeVictorias: 70%;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\fcbarcelona.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""Real Madrid"";" // new_line('A') // &
    "            partidosJugados: 38;" // new_line('A') // &
    "            porcentajeVictorias: 65%;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\realmadrid.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""Manchester United"";" // new_line('A') // &
    "            partidosJugados: 38;" // new_line('A') // &
    "            porcentajeVictorias: 60%;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\manutd.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "    }" // new_line('A') // &
    "    deporte: {" // new_line('A') // &
    "        nombre: ""Baloncesto"";" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""Los Angeles Lakers"";" // new_line('A') // &
    "            partidosJugados: 82;" // new_line('A') // &
    "            porcentajeVictorias: 62%;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\lakers.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""Golden State Warriors"";" // new_line('A') // &
    "            partidosJugados: 82;" // new_line('A') // &
    "            porcentajeVictorias: 70%;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\warriors.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "        equipo: {" // new_line('A') // &
    "            nombre: ""Chicago Bulls"";" // new_line('A') // &
    "            partidosJugados: 82;" // new_line('A') // &
    "            porcentajeVictorias: 55 %;" // new_line('A') // &
    "            logo: ""C:\\imagenes\\bulls.jpg"";" // new_line('A') // &
    "        }" // new_line('A') // &
    "    }" // new_line('A') // &
    "} 34.3 # ?"
    !Inicializar el estado del autómata en 0
    call mi_analizador%inicializarEstado()

    !Analizar la cadena de entrada enviada desde python
    call mi_analizador%analizar(entrada)


end program main