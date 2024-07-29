program logic
    implicit none

    real :: angle

    print *, 'Ingrese un angulo'
    read(*,*) angle

    if (angle .lt. 90.0) then
        print *, 'El angulo es agudo'
    else if (angle .eq. 90.0) then
        print *, 'El angulo es recto'
    else if (angle .lt. 180.0) then
        print *, 'El angulo es obtuso'
    else
        print *, 'El angulo es llano'
    end if

end program logic