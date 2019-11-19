subroutine Create_Table(x,h,table,N,dh)
    USE kernel
    integer :: N,i,j,cout
    real*8 :: x(2,N)
    real*8:: h
    real*8:: dh
    integer:: flag
    integer :: table(N,120)
    
    real*8::neighbour
    real*8::xi(2)
    real*8::xj(2)
    table=0
    
    do i=1,N
        xi=x(1:2,i)
        flag=1
        cout=0
       ! if ((xi(2)<-0.5d0*xi(1)+0.8d0)*(xi(2)>=0.5d0*xi(1)+0.7d0)) then
       !    flag=0
      !  endif
        do j=1,N
        xj=x(1:2,j) 
        
       !   if ((xj(2)<-0.5d0*xj(1)+0.8d0)*(xj(2)>=0.5d0*xj(1)+0.7d0)) then
        !    flag=0
        !  endif
          
        neighbour=Compute_W(xi,xj,h+2*dh,h+2*dh)       
            if ((neighbour>0)*(flag)) then
                    cout=cout+1
                    table(i,cout+1)=j
            endif  
        enddo
        table(i,1)=cout
    enddo

end