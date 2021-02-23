program main
    implicit none
   
    real tstart,xstart,tend,h
    real t,x
    real t0,t1,t2,t3
    real k0,k1,k2,k3
    real f0,f1,f2,f3
   
    real func
   
    character(len=12) fname
   
    write(*,'(a,$)') 'tstart, x(tstart) ? '
    read(*,*) tstart,xstart
   
    write(*,'(a,$)') 'tend, dt ? '
    read(*,*) tend,h
   
    write(*,'(a,$)') 'file name ? '
    read(*,*) fname
   
    open(10,file=fname)
   
    t=tstart
    x=xstart
    do while (t<=tend)
      write(10,'(2f13.5)') t,x
   
      t0=t
      k0=x
      f0=func(t0,k0)
   
      t1=t+h/2.0
      k1=x+f0*h/2.0
      f1=func(t1,k1)
   
      t2=t+h/2.0
      k2=x+f1*h/2.0
      f2=func(t2,k2)
   
      t3=t+h
      k3=x+f2*h
      f3=func(t3,k3)
   
      x=x+(f0+f1*2.0+f2*2.0+f3)*h/6.0
   
      t=t+h
    end do
   
    close(10)
   
  end program main
   
  real function func(t,x)
    implicit none
   
    real t,x
   
    func=(1.0-x**2)
   
    return
  end function func