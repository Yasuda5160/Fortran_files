program main

    implicit none
    integer :: i 
    integer :: fstp
    real(8) :: x,y
    real(8) :: vx, vy
    real(8) :: dt
    real(8) :: a
   
    open(10,file='fortbou.dat')
   
    a    = 1.0d0
    fstp = 1000
    dt   = 0.01d0
   
    call random_number(vx)
    call random_number(vy)
    call random_number(x)
    call random_number(y)
   
    write(10,*) '# x, y'
    write(10,*) x, y
   
    write(10,*)
    write(10,*) 
   
   !----------------------------
   ! loop start (!で始まる部分は処理されません)
   !----------------------------
   do i=1, fstp
   
     x = x + vx*dt
     y = y + vy*dt
   
     if (x >= a) then
      x = a
      vx = -vx
     end if
   
     if(x <= 0 ) then
      x=0.0d0
      vx =-vx
     end if
   
     if (y >= a) then
      y = a
      vy = -vy
     end if
   
     if(y <= 0) then
      y=0.0d0
      vy =-vy
     end if
   
     write(10,'(4e19.9)') x, y
   
     write(10,*)
     write(10,*)
   
   end do
   
   end program