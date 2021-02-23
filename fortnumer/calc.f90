program main
    implicit none
    real(8) a,b,c,d,e,f,g
    
    a=2.0
    b=3.0
    c=a+b
    d=a-b
    e=a*b
    f=a/b
    g=a**b
    
    write(*,*)'a =',a
    write(*,*)'b =',b
    write(*,*)'a+b =',c
    write(*,*)'a-b =',d
    write(*,*)'a*b =',e
    write(*,*)'a/b =',f
    write(*,*)'a**b=',g
    
    end