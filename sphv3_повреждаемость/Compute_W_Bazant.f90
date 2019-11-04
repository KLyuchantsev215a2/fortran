subroutine Compute_W_Bazant(x,h,N,W,table)
    use kernel
     integer :: N,cout,i,j
       real*8 :: x(2,N)
       real*8 :: xi(2)
       real*8 ::xj(2)
       real*8 :: h

       real*8 :: W(N,N)
       real*8 :: ci(N)
       real*8 ::r(2)
       real*8 ::w_tmp
       

        real*8 ::alpha,detcormat_tmp
        integer :: table(N,120)
        
        ci=0 
        W=0
        
        table=0
        do i=1,N
             xi=x(1:2,i)
            cout=0
              do j=1,N
                   xj=x(1:2,j)
                   r=xi-xj
                 
                  w_tmp=(r(1)*r(1)+r(2)*r(2))/(h*h)
                  w_tmp=max(0.0d0,1.0d0-w_tmp)
                  
                  if (w_tmp>0.0d0) then
                    cout=cout+1
                    table(i,cout+1)=j
                    w_tmp=w_tmp*w_tmp 
                    ci(i)=ci(i)+(w_tmp)
                    
                  endif  
              enddo     
              ci(i)=1.0d0/ci(i)
              table(i,1)=cout
              !ci(i)=3.0d0/(3.140d0*h*h)
        enddo
        
        
        do i=1,N
            xi=x(1:2,i)
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xi-xj
                W(i,table(i,j+1))=ci(i)*(1.0d0-(r(1)*r(1)+r(2)*r(2))/(h*h))**2.0d0
            enddo
            
        enddo
        
        return
    end 