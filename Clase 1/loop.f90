program loop
    implicit none

    integer :: i

    !do i = 1, 10
    !print *, i
    !end do

    !i = 1
    !do while (i < 11)
    !print *, i
    !i = i + 1
    !end do

    !do i = 1, 100
    !if (i > 10) then
    !    exit  ! Stop printing numbers
    !end if
    !print *, i
    !end do
    ! Here i = 11

    !Imprimir números pares
    do i = 1, 10
    if (mod(i, 2) == 0) then
        print *, i
    else
        cycle
    end if
    end do

    


end program loop