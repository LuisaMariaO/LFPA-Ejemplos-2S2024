program array_slice
    implicit none
  
    integer :: i
    integer :: array1(10)  ! 1D integer array of 10 elements
    !integer :: array2(10, 10)  ! 2D integer array of 100 elements
  
    !array1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  ! Array constructor
    array1 = [(i, i = 1, 10)]  ! Implied do loop constructor
    !array1(:) = 0  ! Set all elements to zero
    !array1(1:5) = 1  ! Set first five elements to one
    !array1(6:) = 1  ! Set all elements after five to one
  
    !print *, array1(1:10:2)  ! Print out elements at odd indices
    !print *, array2(:,1)  ! Print out the first column in a 2D array
    print *, array1(10:1:-1)  ! Print an array in reverse

    !print *, array1
  
  end program array_slice