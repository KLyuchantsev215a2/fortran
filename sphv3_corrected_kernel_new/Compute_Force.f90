subroutine Compute_Force(x,x_init,F,thichness,U,cor_W,disp,Force,Ci,count_hole,count_section,vol,nabla_W_0_1,nabla_W_0_2,N,mu,k,table,index_hole,index_section,pi,time_calculated)
    !input x,x_inint
    !output Force
    IMPLICIT NONE
    integer :: N,i
    integer ::k2,k1,count_hole,count_section
    real*8 :: x(2,N)
    real*8 :: x_init(2,N)
    
    real*8 :: F(3,3,N)
    real*8 :: thichness(N)
    real*8 :: cor_W(N,N)
    real*8 :: U(N)
    real*8 :: disp,per
    real*8 :: Ci(3,3,N)
    
    real*8 :: nabla_W_0_1(N,N)
    real*8 :: nabla_W_0_2(N,N)
    real*8 :: mu
    real*8 :: k
    real*8 :: pi,time_calculated
    real*8 :: vol
    
    integer :: table(N,120)
    integer :: index_hole(count_hole)
    integer :: index_section(count_section)
    
    real*8 ::Poten
    real*8 :: Force
    !начало блок вычисления силы
    Poten=0
    per=1.0d-6

     
    call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)  
    
    
    call Compute_Potential(F,thichness,Ci,U,mu,k,N)   
    
    do i=1,N  
            Poten=Poten+U(i)*vol
    enddo 
    
    Force=Poten   
    
     do k2=1,count_hole
        x(2,index_hole(k2))=x(2,index_hole(k2))+per
    enddo  
 
    
    call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)    
    
    Poten=0
    call Compute_Potential(F,thichness,Ci,U,mu,k,N)    
    
    do i=1,N
            Poten=Poten+U(i)*vol
    enddo     
    
    Force=(Poten-Force)/(per) 
    
   do k2=1,count_hole
        x(2,index_hole(k2))=x(2,index_hole(k2))-per
    enddo      
    !конец блок вычисления силы

end