subroutine Compute_Acceleration(x,Ci,s,acc,PK1,F,Couchy,thichness,table,x_init,cor_W,nabla_W_0_1,nabla_W_0_2,mu,k,eta,vol,YieldStress,betar,gammar,betas,gammas,rho_0,dt,N) 
  IMPLICIT NONE
    integer :: N,i,j,alpha,flag
    real*8 :: dt
    real*8 :: mu
    real*8 :: k
    real*8 :: eta
    real*8 :: YieldStress
    real*8 :: vol
    real*8 :: betar
    real*8 :: gammar
    real*8 :: betas
    real*8 :: gammas
    real*8 :: rho_0
    real*8 :: s(N)
    real*8 :: s_new(N)
    real*8 :: F(3,3,N)
    real*8 :: Ci(3,3,N)
    real*8 :: thichness(N)
    real*8 :: Couchy(3,3,N)
    real*8 :: PK1(3,3,N)
    real*8 :: x(2,N)
    real*8 :: x_init(2,N)
    real*8 :: cor_W(N)
    real*8 :: nabla_W_0_1(N,N)
    real*8 :: nabla_W_0_2(N,N)
    real*8 :: acc(2,N)
    integer :: table(N,120)
    flag=1
    call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
    call  OneStepPlasticity(F,s,Ci,thichness,Couchy,PK1,mu,k,eta,dt,YieldStress,gammar,betar,gammas,betas,N,flag)   
    
    acc=0.0d0
    do i=1,N
        do j=1,table(i,1)
                do alpha=1,2  
                    acc(alpha,i)=acc(alpha,i)-vol*PK1(alpha,1,table(i,j+1))*nabla_W_0_1(table(i,j+1),i)
                    acc(alpha,i)=acc(alpha,i)-vol*PK1(alpha,2,table(i,j+1))*nabla_W_0_2(table(i,j+1),i)
                enddo
        enddo
        do alpha=1,2
            acc(alpha,i)=acc(alpha,i)/rho_0
        enddo
    enddo
    return
end