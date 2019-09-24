subroutine Compute_W_cor(x,xper,h,N,vol,W,table)
    use kernel
     integer :: N
       real*8 :: x(2,N)
       real*8 :: xper(2,N)
       real*8 :: h
       real*8 :: vol
       real*8 :: W(N,N)
        
        real*8 ::xi(2)
        real*8 ::xj(2)
        real*8 ::r(2)
        real*8 ::sumW(2)
        real*8 ::betaij(2)
        real*8 ::cormat(2,2)
        real*8 ::cormat_tmp(2,2)
        real*8 ::alpha,detcormat_tmp
        integer :: table(N,120)
        
        W=0
        do i=1,N
            sumW=0
            alpha=0
            betaij=0
            cormat=0
            cormat_tmp=0
            xi=xper(1:2,i)
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xj-xi
                sumW(1)=sumW(1)+vol*Compute_W(xi,xj,h,h)*r(1)
                sumW(2)=sumW(2)+vol*Compute_W(xi,xj,h,h)*r(2)
            enddo
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xi-xj
                cormat_tmp(1,1)=cormat_tmp(1,1)+r(1)*r(1)*vol*Compute_W(xi,xj,h,h)
                cormat_tmp(1,2)=cormat_tmp(1,2)+r(1)*r(2)*vol*Compute_W(xi,xj,h,h)
                cormat_tmp(2,1)=cormat_tmp(2,1)+r(2)*r(1)*vol*Compute_W(xi,xj,h,h)
                cormat_tmp(2,2)=cormat_tmp(2,2)+r(2)*r(2)*vol*Compute_W(xi,xj,h,h)
            enddo
            detcormat_tmp=cormat_tmp(1,1)*cormat_tmp(2,2)-cormat_tmp(1,2)*cormat_tmp(2,1)
            cormat(1,1)=cormat_tmp(2,2)/detcormat_tmp
            cormat(1,2)=-cormat_tmp(1,2)/detcormat_tmp
            cormat(2,1)=-cormat_tmp(2,1)/detcormat_tmp
            cormat(2,2)=cormat_tmp(1,1)/detcormat_tmp
            
            betaij(1)=cormat(1,1)*sumW(1)+cormat(1,2)*sumW(2)
            betaij(2)=cormat(1,2)*sumW(1)+cormat(2,2)*sumW(2)
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xi-xj
                alpha=alpha+vol*(1.0+betaij(1)*r(1)+betaij(2)*r(2))*Compute_W(xi,xj,h,h)
            enddo
            alpha=1.0d0/alpha
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xi-xj
                W(i,table(i,j+1))=Compute_W(xi,xj,h,h)*alpha*(1.0d0+betaij(1)*r(1)+betaij(2)*r(2))
            enddo
            
        enddo
        return
    end 