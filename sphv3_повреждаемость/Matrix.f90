MODULE matrix
  implicit none
  CONTAINS
    subroutine inv_matrix (M,invM)
      real*8:: M(3,3)
      real*8:: invM(3,3)
      
        invM(1,1)=M(2,2)/((M(1,1)*M(2,2)-M(1,2)*M(2,1)))
        invM(1,2)=-M(1,2)/((M(1,1)*M(2,2)-M(1,2)*M(2,1)))
        invM(2,1)=-M(2,1)/((M(1,1)*M(2,2)-M(1,2)*M(2,1)))
        invM(2,2)=M(1,1)/((M(1,1)*M(2,2)-M(1,2)*M(2,1)))
        invM(3,3)=1/M(3,3)  ! inverse of F ready!
    
    end 
    
    subroutine trans (M,transM)
        integer::alpha,beta
      real*8 :: M(3,3)
      real*8 :: transM(3,3)
        
        do alpha=1,3
            do beta=1,3
                transM(alpha,beta)=M(beta,alpha)
            enddo
        enddo
            
    end 
    
    subroutine dev (M,devM)
            integer::alpha,beta
          real*8:: M(3,3)
          real*8:: devM(3,3)
          real*8::trace
            devM=M
            trace=M(1,1)+M(2,2)+M(3,3)
            do alpha=1,3
                devM(alpha,alpha)=devM(alpha,alpha)-(1.0d0/3.0d0)*(trace)
            enddo
             
    end
    
    subroutine mymulty (M,M1,multyMM1)
        integer::alpha,bbeta,ggamma
      real*8 :: M(3,3)
      real*8 :: M1(3,3)
      real*8 :: multyMM1(3,3)
        multyMM1=0.0d0
         do alpha=1,3
            do bbeta=1,3
                multyMM1(alpha,bbeta)=0
                do ggamma=1,3
                    multyMM1(alpha,bbeta)=multyMM1(alpha,bbeta)+M(alpha,ggamma)*M1(ggamma,bbeta)
                enddo
            enddo
        enddo
            
    end 
    
    function det (M)
       real*8 :: M(3,3)
       real*8 ::det
         det=(M(1,1)*M(2,2)-M(1,2)*M(2,1))*M(3,3)
         
    end function det
        
    function trace (M)
          real*8 :: M(3,3)
          real*8 :: trace
            trace=M(1,1)+M(2,2)+M(3,3)
    end function trace
    
END MODULE Matrix