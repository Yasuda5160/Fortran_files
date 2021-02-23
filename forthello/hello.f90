program hello
    implicit none

    integer :: i
    character(len=16) :: nam

    print *, 'What is your name?'
    read *, nam

    do i=0,200
        print *, 'Hello, ',nam,'!'   
    end do

end program hello