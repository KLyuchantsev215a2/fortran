subroutine Compute_Potential(F,thichness,Ci,U,mu,k,N)
! input:  F = deformation gradient
!         mu = shear modulus
!          k = bulk modulus
!       Ci=inelastic part Cauchy-Green
!
!  output: U potential energy

use matrix 

integer:: N
real*8 :: F(3,3,N)
real*8 :: thichness(N)
real*8 :: Ci(3,3,N)
real*8 :: U(N)
real*8 :: k
real*8 :: mu


real*8 :: J
real*8 :: detA
real*8 :: Aiso(3,3)
real*8 :: RCG(3,3)
real*8 :: RCGiso(3,3)
real*8 :: Fp(3,3)
real*8 :: Ft(3,3)
real*8 :: Cip_e(3,3)
real*8 :: invCi_e(3,3)
real*8 :: RCGinvC(3,3)


U=0
do i=1,N  
    Fp(1:3,1:3) = F(1:3,1:3,i)
    
    Cip_e(1:2,1:2)=Ci(1:2,1:2,i)     ! take old C_i
    Cip_e(3,3)=1.0/(Ci(1,1,i)*Ci(2,2,i)-Ci(1,2,i)*Ci(2,1,i))
        
    J = (Fp(1,1)*Fp(2,2)-Fp(1,2)*Fp(2,1))*Fp(3,3)
    
    call inv_matrix(Cip_e,invCi_e)   !Ci^(-1) 
    
    call trans(Fp,Ft)  !F'
    
    call mymulty(Ft,Fp,RCG) !RCG=F'*F = C
    
    RCGiso = J**(-2.0d0/3.0d0)*RCG    !isochoric part of  right Cauchy–Green deformation tensor
    
    call mymulty(RCGiso,invCi_e,RCGinvC)! Ciso*Ci^(-1) A
    
    Aiso=RCGinvC
    
    U(i)=mu/2.0d0*(Aiso(1,1)+Aiso(2,2)+Aiso(3,3)-3)+k/50.0d0*(J**(5.0d0)+J**(-5.0d0)-2) !mu/2*(trace(C_iso*Ci^(-1))+k/50*(J^5-J^(-5)-2)
enddo

end