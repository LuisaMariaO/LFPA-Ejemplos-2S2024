program arithmetic
    implicit none
  
    real :: pi
    real :: radius
    real :: height
    real :: area
    real :: volume
  
    pi = 3.1415927
  
    print *, 'Enter cylinder base radius:'
    read(*,*) radius
  
    print *, 'Enter cylinder height:'
    read(*,*) height
  
    area = pi * radius**2
    volume = area * height
  
    print *, 'Cylinder radius is: ', radius
    print *, 'Cylinder height is: ', height
    print *, 'Cylinder base area is: ', area
    print *, 'Cylinder volume is: ', volume
  
  end program arithmetic

