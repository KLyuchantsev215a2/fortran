subroutine Compute_Newton_Fluid(F,CN,CN_new,PK1N,N,dt,etaN)
    use matrix
    !input:
    !     CN: previous C for the Newton fluid
    !  C_new: current C
    !      F: current deformation gradient for the Newton fluid
    !output: 
    !   PK1N: 1st PK stress tensor
    ! CN_new: current right Cauchy-Green tensor for the Newton fluid
    !
     integer :: N
     real*8 :: F(3,3,N)
     real*8 ::invFp(3,3)
     real*8 :: etaN
     real*8 :: CN(3,3,N)
     real*8 :: CN_new(3,3,N)
     real*8 :: Cpp(3,3)
     real*8 :: Cpp_new(3,3)
     real*8 :: Fp(3,3)
     real*8 :: dt
     real*8 :: PK1N(3,3,N)
     real*8 :: trans_Fp(3,3)
     real*8 :: trans_inv_Fp(3,3)
     real*8 :: PK2N(3,3)
    

     real*8 :: invC_new(3,3)
     real*8 :: invC(3,3)
     real*8 :: C_inv_point(3,3)
    
   
     real*8 :: XN(3,3)
    
     real*8 ::multXNC(3,3)

    
     do i=1,N
         Fp=0
         Cpp(1:3,1:3)=CN(1:3,1:3,i)  ! C from the previous time step
         Fp(1:3,1:3)=F(1:3,1:3,i)    ! current F 
         
         call inv_matrix(Cpp,invC)   !inv nC   (old)
         
        
         call inv_matrix(Fp,invFp)   ! inv of current F
         
         call trans(Fp,trans_Fp)
         
         call inv_matrix(trans_Fp, trans_inv_Fp)
         
         call mymulty(trans_Fp,Fp,Cpp_new)    !  now C = F' F   - current C
         
         
         call inv_matrix(Cpp_new,invC_new) !inv n+1 C
         
         C_inv_point=1.0d0/dt*(invC_new-invC) ! dot C
        
         CN_new(1:3,1:3,i)=Cpp_new(1:3,1:3)
         
         XN=-etaN*C_inv_point(1:3,1:3)
        
         call mymulty(XN,Cpp_new,multXNC)
         
         PK2N(1:3,1:3)=XN(1:3,1:3)-1.0d0/3.0d0*(multXNC(1,1)+multXNC(2,2)+multXNC(3,3))*invC_new(1:3,1:3)
         
         do alpha=1,3
            do bbeta=1,3
               PK1N(alpha,bbeta,i)=0
                do ggamma=1,3
                   PK1N(bbeta,alpha,i)=PK1N(bbeta,alpha,i)+PK2N(ggamma,bbeta)*trans_inv_Fp(alpha,ggamma)
                enddo
            enddo
         enddo
         
         
         
         
         
    enddo
end