subroutine Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
!input x,x_int              
!output F
    integer :: N
    real*8 :: vol
    real*8 :: x(2,N)
    real*8 :: x_init(2,N)
    real*8 :: cor_W(N,N)
    real*8 :: nabla_W_0_1(N,N)
    real*8 :: nabla_W_0_2(N,N)
    real*8 ::thichness(N)
    real*8 :: F(3,3,N)
    real*8 :: ui,uj
    
    integer :: table(N,120)
    F=0
        do i=1,N
            do j=1,table(i,1)
              
                    do alpha=1,2
                        uj=x(alpha,table(i,j+1))-x_init(alpha,table(i,j+1))
                        ui=x(alpha,i)-x_init(alpha,i)
                        F(alpha,1,i)=F(alpha,1,i)+vol*(uj-ui)*nabla_W_0_1(i,table(i,j+1))
                        F(alpha,2,i)=F(alpha,2,i)+vol*(uj-ui)*nabla_W_0_2(i,table(i,j+1))
                    enddo
                    
                    F(3,3,i)=F(3,3,i)+vol*cor_W(i,table(i,j+1))*thichness(table(i,j+1))

            enddo
            
            F(1,1,i)= F(1,1,i)+1.0d0
            F(2,2,i)= F(2,2,i)+1.0d0
        enddo
        
    return
end