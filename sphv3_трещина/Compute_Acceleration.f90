subroutine Compute_Acceleration(x,Ci,s,acc,PK1,F,Fedge,Cauchy,x_init,thichness,table,table_non_local,cor_W,cor_W1,nabla_W_0_1,nabla_W_0_2,mu0,k0,eta,mu,k,gammar,phi,etaN,vol,YieldStress,YieldStress0,betar,gammar0,rho_0,dt,N) 
  !input: 
  !     x     position vector
  !     Ci,s  internal varaibles of viscoplastic model
  !     CN    old right Cauchy-Green tensor for Newton fluid. Computed using SPH with h = h_N
  !const: table,x_init,cor_W,nabla_W_0_1,nabla_W_0_2,mu,k,eta,etaN,vol,YieldStress,betar,gammar,betas,gammas,rho_0,dt,N
  !ouputput: 
  !      acc   acceleration vector
  !   CN_new   current right Cauchy-Green tensor for Newton fluid. Computed using SPH with h = h_N
    
    IMPLICIT NONE
    integer :: N,i,j,alpha,flag
    real*8 :: dt
    real*8 :: mu0
    real*8 :: k0
    real*8 :: eta
    real*8 :: etaN
    real*8 :: YieldStress(N)
    real*8 :: YieldStress0(N)
    real*8 :: mu(N)
    real*8 :: k(N)
    real*8 :: phi(N)
    real*8 :: gammar(N)
    real*8 :: vol
    real*8 :: betar
    real*8 :: gammar0
    real*8 :: betas
    real*8 :: gammas
    real*8 :: rho_0
    real*8 :: s(N)
    real*8 :: Fedge(2)!edge Force=reaction force
    real*8 :: s_new(N)
    real*8 :: F(3,3,N)
    real*8 :: FN(3,3,N)
    real*8 :: Ci(3,3,N)
    real*8 :: thichness(N)
    real*8 :: Cauchy(3,3,N)
    real*8 :: PK1(3,3,N)
    real*8 :: x(2,N)
    real*8 :: x_init(2,N)
    real*8 :: cor_W(N,N)
    real*8 :: cor_W1(N,N)
    real*8 :: nabla_W_0_1(N,N)
    real*8 :: nabla_W_0_2(N,N)
    real*8 :: acc(2,N)
    integer :: table(N,120)
    integer :: table_non_local(N,1800)
    flag=1
    
    call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
    
    
    call  OneStepPlasticity(F,s,Ci,thichness,Cauchy,PK1,mu0,k0,eta,mu,k,gammar,phi,dt,YieldStress,YieldStress0,gammar0,betar,N,flag,table,table_non_local,cor_W1,vol,x)   
    
    Fedge=0.0d0
    acc=0.0d0
    
    do i=1,N
        do j=1,table(i,1)
                do alpha=1,2  
                    acc(alpha,i)=acc(alpha,i)-vol*(PK1(alpha,1,table(i,j+1)))*nabla_W_0_1(table(i,j+1),i)
                    acc(alpha,i)=acc(alpha,i)-vol*(PK1(alpha,2,table(i,j+1)))*nabla_W_0_2(table(i,j+1),i)
                enddo
        enddo
        
        
     !    if(x_init(2,i)>=1.44d0) then
       !             do alpha=1,2
       !             Fedge(alpha)=Fedge(alpha)-acc(alpha,i)*vol
      !              enddo
      !  endif
        
        do alpha=1,2
            acc(alpha,i)=acc(alpha,i)/rho_0
        enddo
    enddo
    return
end