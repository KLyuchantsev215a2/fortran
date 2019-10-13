subroutine Compute_Newton_Fluid(F,etaN,CN,CN_new,PK1N,N,dt)
    !input C-previous C
    !C_new current C
    !F
    !output PK1 Newton
     integer :: N
     real*8 :: F(2,2,N)
     real*8 ::invFp(3,3)
     real*8 :: etaN
     real*8 :: CN(3,3,N)
     real*8 :: CN_new(3,3,N)
     real*8 :: Cpp(3,3)
     real*8 :: Fp(3,3)
     real*8 :: dt
     real*8 :: PK1N(3,3,N)
     real*8 :: trans_Fp(3,3)
     real*8 :: PK2N(3,3)
    

     real*8 :: invC_new(3,3)
     real*8 :: invC(3,3)
     real*8 :: C_inv_point(3,3)
    
   
     real*8 :: XN(3,3)
    
     real*8 ::multXNC(3,3)
     real*8 :: trace
    
     do i=1,N
         Fp=0
         Cpp(1:3,1:3)=C(1:3,1:3,i)   ! C from the previous time step
         Fp(1:2,1:2)=F(1:2,1:2,i)    ! current F 
         
         call inv_matrix(Cpp,invC)   !inv nC
         
         Fp(3,3)=1
        
         call inv_matrix(Fp,invFp)
         
         call trans(Fp,trans_Fp)
           
         call mymulty(trans_Fp,Fp,Cpp)    !  now C = F' F   - current C
         
         
         call inv_matrix(Cpp,invC_new) !inv n+1 C
         
         C_inv_point=1.0d0/dt*(invC_new-invC) !pointC
        
         CN_new(1:3,1:3,i)=Cpp(1:3,1:3)
         
         XN=-etaN*C_inv_point(1:3,1:3)
        
         call mymulty(XN,Cpp,multXNC)
         
         PK2N(1:3,1:3)=XN(1:3,1:3)-1.0d0/3.0d0*(multXNC(1,1)+multXNC(2,2)+multXNC(3,3))*C_inv_point(1:3,1:3)
         
         do alpha=1,3
            do bbeta=1,3
               PK1N(alpha,bbeta,i)=0
                do ggamma=1,3
                   PK1N(bbeta,alpha,i)=PK1N(bbeta,alpha,i)+invFp(alpha,ggamma)*PK2N(ggamma,bbeta)
                enddo
            enddo
         enddo
         
         
         
         
         
    enddo
end