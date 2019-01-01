program sph

real rho,T,dt,sqn,m,nu,mu,k,h,dh,E,CFL
integer N
common F,C,W,nabla_W,vol,x,v,x_old

dimension F(:,:,:),C(:,:,:),W(:,:),nabla_W(:,:),vol(:),x(:,:),v(:,:)

interface
      function F_init()
      implicit real*8 (a-h,o-z)
      real, ALLOCATABLE :: F(:,:,:) !rank 2
      end function F_init
end interface

open (unit=1, file="input_sph.txt", status='old',    &
             access='sequential', form='formatted', action='read' )

read (1, 1100) rho_0,T,nu,mu,S,dh,CFL,N
write (*, 1100) rho_0,T,nu,mu,S,dh,CFL,N

m=rho_0*S/N
k=2.0*mu*(1+nu)/(3.0*(1.0-2.0*nu))
h=1.4*sqrt((m/rho_0))
E=9*k*mu/(3.0*k+mu)
dt=CFL*h/(cs_0+v_0)
pause
1100 format (7f10.0,1i4)
F=init(N) 

end program sph
    
function F_init(N)
      implicit real*8 (a-h,o-z)
      dimension F(:,:,:)
      
        allocate(F(2,2,N))
        do i=1,N
           F(1,1,i)=1
           F(2,2,i)=1
        enddo
        deallocate(F)
        F_init=F
      end function F_init! end of xprod