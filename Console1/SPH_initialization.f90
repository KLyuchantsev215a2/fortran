program geometry

 
    real*8::Nx,Ny,Nxl,tetta
    integer::i,yi,xi,Nreal,flag,x1old
    real*8::rho_0, v_0, T, Lx, Ly
    real*8::bias,Ax,Bx,Ay,By,x0,y0
    
 
    real*8, allocatable :: x(:,:)
    real*8, allocatable :: xn(:,:)
    real*8, allocatable :: h(:)
    real*8, allocatable :: v(:,:)
    
    open (unit=1, file="input.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
    open (unit=2, file="output.txt")
    
   ! Nx=50
   ! Ny=55
    !Nxl=63
    
   ! bias=1.0d0
! Lx=1.25d0-0.7d0
  !  Ly=0.6d0
    tetta=1.22173
    Bx=((bias-1.0d0)*Lx)/(Nx*(Nx*bias-1.0d0-bias))
    Ax=(Lx-Bx*Nx*Nx)/Nx
    
    By=((bias-1.0d0)*Ly)/(Ny*(Ny*bias-1.0d0-bias))
    Ay=(Ly-By*Ny*Ny)/Ny
    
    Nreal=150*150
    x0=0.0d0
    y0=0.0d0
        allocate(x(2,Nreal))
         allocate(xn(2,Nreal))
        !h=S/N
        
     !   i=0
      !  do xi=0,int(Nx)
      !      do yi=0,int(Ny) 
      !          i=i+1
               ! h(i)=
       !         x(1,i) = Ax*xi+Bx*xi*xi
       !         x(2,i) = Ay*yi+By*yi*yi
                
       !     enddo
      !  enddo
        
      !  do xi=0,int(Nx)
      !      do yi=1,int(Ny) 
      !          i=i+1
     !           x(1,i) = Ax*xi+Bx*xi*xi
      !          x(2,i) =-(Ay*yi+By*yi*yi)
     !           
     !       enddo
     !   enddo
        
     !   Lx=0.7d0
     !   Bx=((bias-1.0d0)*Lx)/(Nxl*(Nxl*bias-1.0d0-bias))
     !   Ax=(Lx-Bx*Nxl*Nxl)/Nxl
    
     !    do xi=1,int(Nxl)
     !       do yi=1,int(Ny) 
     !           i=i+1
      !          x(1,i) =-(Ax*xi+Bx*xi*xi)
      !          x(2,i) = Ay*yi+By*yi*yi 
      !      enddo
     !    enddo
         
         
         
      !     do xi=1,int(Nxl)
      !      do yi=1,int(Ny) 
       !         i=i+1
       !         x(1,i) =-(Ax*xi+Bx*xi*xi)
      !          x(2,i) =-(Ay*yi+By*yi*yi) 
      !      enddo
     !   enddo
        
        i=1
        do yi=1,150
            do xi=1,150
                flag=1
      
                x(1,i) = real(xi-1)/real(150-1)*6.0d0-3.0d0
                x(2,i) = real(yi-1)/real(150-1)*6.0d0-3.0d0
                xn(1,i)=x(1,i)*cos(tetta)-x(2,i)*sin(tetta)+3.0d0
                xn(2,i)=x(1,i)*sin(tetta)+x(2,i)*cos(tetta)+3.0d0
             
                i=i+1
            enddo
        enddo
        
      !  do i=1,Nx
            

        !    write (2,1110) i,x(1,i),x(2,i)
            
         !    if( x(2,i)>0) then
         !        write (2,1110) i,x(1,i),-x(2,i)
         !       endif
            
       ! enddo
       
        
        

        
        
      !  deallocate(v)
          
        
       do i=1,Nreal
           flag=0
       if ((xn(2,i)<=2.5d0)*(xn(2,i)>=1.0d0)*(xn(1,i)>=2.0d0)*(xn(1,i)<=3.0)) then
        flag=1
       endif
       !  if (sqrt((0.5d0-x(1,i))**2+(0.75d0-x(2,i))**2)>=0.5d0) then
!           flag=0
 !       endif
             
        if(flag==1) then
            write (2,1110) i,xn(1,i)-2.0d0,xn(2,i)-1.0d0
       endif
        
       enddo
     !   call  plot_init(x,N)
        call plot_init(xn,Nreal)
        deallocate(x)
        !pause
      
1100 format (4f10.0,1i3,1i1)
1110 format (1i5,2f)
    
end program geometry 