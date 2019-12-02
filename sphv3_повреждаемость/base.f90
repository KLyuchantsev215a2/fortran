program base
!input:: const material, coordinats, краевые условия
!ouput:: tensor stress PK1, coordinats, Force 
 IMPLICIT NONE
integer N,i,j,cap_down,cap_up,k1,k2,a,count_section,count_hole,flag,flag1!the number of particles that sample the environment
integer step!counter for time steps
real*8 :: dt,time_calculated!time step, time during calculation
integer fr,coutfr! for frame
    
real*8 :: T,h_non_local!density, total calculation time ,the size of the side of the square, Courant number
    
real*8 :: Area,m,rho_0,nu,eta,betar,etaN,SRR,BRR,IRR,mu0,k0,gammar0,A_phi,dgrowth ! const materials and body
    
real*8 :: dh!indent for calculating the derived kernel through finite differences
real*8 :: pi!const Pi
    
real*8 :: Force,Force_old,Fedge(2)!Сила 
real*8 :: Kin,Poten,maxPK133,maxthichness,damp_thick!Energy 
    
integer, allocatable :: table(:,:)!таблица связности
integer, allocatable :: table_non_local(:,:)!таблица связности
real*8, allocatable :: x(:,:)
real*8, allocatable :: v(:,:)
real*8, allocatable :: acc(:,:)
    
real*8, allocatable :: xplot(:,:,:)!для гиф анимации 
real*8, allocatable :: x_init(:,:)
real*8, allocatable :: YieldStress(:)
real*8, allocatable :: YieldStress0(:)
real*8, allocatable :: mu(:)
real*8, allocatable :: k(:)
real*8, allocatable :: phi(:)
real*8, allocatable :: gammar(:)
real*8, allocatable :: s(:)
real*8, allocatable :: s_new(:)

real*8, allocatable :: W(:,:)
real*8, allocatable :: cor_W(:,:)
real*8, allocatable :: cor_W1(:,:)
real*8, allocatable :: Wper1(:,:)
real*8, allocatable :: Wper2(:,:)
real*8, allocatable :: nabla_W_0_1(:,:)
real*8, allocatable :: nabla_W_0_2(:,:)
real*8, allocatable :: WN(:,:)
    
real*8, allocatable :: Cauchy(:,:,:)
real*8, allocatable :: Cauchy_per(:,:,:)
real*8, allocatable :: PK1(:,:,:)
real*8, allocatable :: PK1_per(:,:,:)
real*8, allocatable :: F(:,:,:)
real*8, allocatable :: Ci(:,:,:)
real*8, allocatable :: Ci_new(:,:,:)

    
real*8, allocatable :: U(:)
real*8:: disp
real*8:: per
real*8, allocatable :: thichness(:)
real*8, allocatable :: deriv(:)

real*8:: vol
real*8 :: h
real*8:: hN

integer, allocatable :: index_section(:)
integer, allocatable :: index_hole(:)


        
    open (unit=1, file="10.txt")
    open (unit=2, file="Force_SPH.txt", action='write')
    open (unit=3, file="s.txt", action='write')
    open (unit=4, file="phi_1.txt", action='write')
    
    read (1, 1100) rho_0, T,nu, mu0,k0,gammar0,betar,eta,pi, dh,N  
    write (*, 1100) rho_0, T,nu, mu0,k0,gammar0,betar,eta,pi, dh,N

    pi=3.14159265359
    Area=1.5d0
    coutfr=1
    per=0.000005d0
    m=rho_0*Area/N  
    vol=m/rho_0
    h=1.0*sqrt(m/rho_0)
    h_non_local=0.0000002d0
    dt=1.0d-5
    disp=0.01d0
    damp_thick=1.0d0
    fr=int(T/dt/50)
    
    allocate(x(2,N))
    allocate(v(2,N))
    allocate(acc(2,N))
    allocate(x_init(2,N))
    allocate(xplot(2,N,800))

    allocate(table(N,120))
    allocate(table_non_local(N,1800))
    allocate(s(N))
    allocate(mu(N))
    allocate(k(N))
    allocate(phi(N))
    allocate(gammar(N))
    allocate(s_new(N))
    allocate(YieldStress0(N))
    allocate(YieldStress(N))
    YieldStress0=335.0d0
    allocate(cor_W(N,N))
    allocate(W(N,N))
    allocate(Wper1(N,N))
    allocate(nabla_W_0_1(N,N))
    allocate(nabla_W_0_2(N,N))
    
    mu(1:N)=mu0
    k(1:N)=k0
    gammar(1:N)=gammar0
    !начало блок инициализации начальных данных
    do i=1,N
        read (1, 1110) a,x(1,i),x(2,i) !read coordinats
    enddo
   
    call Create_Table(x,h,table,N,dh) 
    !начало блок краевых условий
    count_hole=0
    count_section=0
     do i=1,N
        if(x(2,i)>=1.0d0) then
            YieldStress0(i)=805.0d0
        end if
        if (x(2,i)<=0.5d0) then
            YieldStress0(i)=805.0d0
        end if  
    enddo
    YieldStress=YieldStress0
    
    do i=1,N
        if(x(2,i)>=1.48d0) then
            count_hole=count_hole+1
        end if
        if (x(2,i)<=0.02d0) then
            count_section=count_section+1
        end if  
    enddo
    
    allocate(index_hole(count_hole))
    allocate(index_section(count_section)) 
    k1=1
    k2=1
    do i=1,N 
     if(x(2,i)>=1.48d0) then  
                index_hole(k1)=i
                k1=k1+1
        end if
        if (x(2,i)<=0.02d0) then
                index_section(k2)=i                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                k2=k2+1
        end if
    enddo
    !конец блок краевых условий
    
    x_init=x
    v=0

    call Compute_W_cor(x,x,h,N,vol,cor_W,table)
    call Compute_W_Bazant(x,h_non_local,N,W,table_non_local)
    call Compute_nabla_W(x,h,vol,N,Wper1,nabla_W_0_1,nabla_W_0_2,dh,table)!tmp
   
   
    allocate(F(3,3,N))
    allocate(thichness(N))
    thichness=1.0d0
    allocate(deriv(N))
    allocate(Ci(3,3,N))
    allocate(Ci_new(3,3,N))
    allocate(Cauchy(3,3,N))
    allocate(Cauchy_per(3,3,N))
    allocate(PK1(3,3,N))
    allocate(PK1_per(3,3,N))
    allocate(U(N))
   
   call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
  
   s=0.0d0
   
   
   phi=1.0d0

   Ci=0.0d0
   Ci(1,1,1:N)=1
   Ci(2,2,1:N)=1
   Ci(3,3,1:N)=1
   flag=0.0d0
   call OneStepPlasticity(F,s,Ci,thichness,Cauchy,PK1,mu0,k0,eta,mu,k,gammar,phi,dt,YieldStress,YieldStress0,gammar0,betar,N,flag,table,table_non_local,W,vol,x) 
   !Ci(1:3,1:3,1:N)=Ci_new(1:3,1:3,1:N)
   acc=0.0d0
   Force=0.0d0
   maxPK133=0.0d0
   flag1=0.0d0
   !конец блок инициализации начальных данных
   call plot_init(x,N,count_hole,count_section,index_section,index_hole)
 !  call surf(s,N)
!начало блок интегрирования по времени
do step=1,int(T/dt)
    
    v=v+dt*acc
    x=x+dt*v
    
    time_calculated=(real(step-1)*dt)
    
  !  if((time_calculated>=0.1d0)*(flag1==0.0d0)) then
  !      flag1=1.0d0
  !       flag=0.0d0
   !      x_init=x
   !      acc=0
    !     v=0
         
     !    call Create_Table(x,h,table,N,dh) 
    
      !   call Compute_W_cor(x,x,h,N,vol,cor_W,table)
      !   call Compute_nabla_W(x,h,vol,N,Wper1,nabla_W_0_1,nabla_W_0_2,dh,table)!tmp
      !   call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
       !     s=0.0d0
      !      Ci=0.0d0
       !     Ci(1,1,1:N)=1
      !      Ci(2,2,1:N)=1
      !      Ci(3,3,1:N)=1
            
       ! call OneStepPlasticity(F,s,Ci,thichness,Cauchy,PK1,mu,k,eta,dt,YieldStress,gammar,betar,gammas,betas,N,flag)  
   ! end if
   
    !flag=1
    do k2=1,count_hole
        x(2,index_hole(k2))=x_init(2,index_hole(k2))+disp*(1-cos(pi*time_calculated))
        x(1,index_hole(k2))=x_init(1,index_hole(k2))
    enddo  
        
    do k1=1,count_section
        x(2,index_section(k1))=x_init(2,index_section(k1))-disp*(1-cos(pi*time_calculated))
        x(1,index_section(k1))=x_init(1,index_section(k1))
    enddo
    
  
   ! call Compute_F(x,x_init,thichness,F,vol,cor_W,nabla_W_0_1,nabla_W_0_2,N,table)
  !  
   !  do j=1,1
   ! flag=0
    
   !  call  OneStepPlasticity(F,s,Ci,thichness,Cauchy,PK1,mu,k,eta,dt,YieldStress,gammar,betar,gammas,betas,N,0) 
   ! call  OneStepPlasticity(F,s,Ci,thichness,Cauchy,PK1_per,mu,k,eta,dt,YieldStress,gammar,betar,gammas,betas,N,0)
   !  do i=1,N
   !      deriv(i)=(Cauchy_per(3,3,i)-Cauchy(3,3,i))/per
    !     thichness(i)=thichness(i)-Cauchy(3,3,i)/deriv(i)
   !  enddo
    ! enddo
     
   ! do i=1,N
   !    thichness(i)=thichness(i)-Cauchy(3,3,i)*damp_thick*dt
  !  enddo
     
    
    
    !вычисление ускорения
    call Compute_Acceleration(x,Ci,s,acc,PK1,F,Fedge,Cauchy,x_init,thichness,table,table_non_local,cor_W,W,nabla_W_0_1,nabla_W_0_2,mu0,k0,eta,mu,k,gammar,phi,etaN,vol,YieldStress,YieldStress0,betar,gammar0,rho_0,dt,N) 
    
  !  CN=CN_new
    
    !запоминание фрейма
     if(step-int(step/fr)*fr==0) then
       ! Force_old=0
        Force=0
       !  call Compute_Potential(F,thichness,Ci,U,mu,k,N)
       ! do i=1,N
       !     if(abs(x_init(2,k1)-0.762)<=0.001d0) then 
            
       !         if((x_init(1,k1)<=0.0000001)+(x_init(1,k1)>=0.9999)) then
       !            PK1(2,2,k1)=PK1(2,2,k1)/2.0d0
       !         endif
       !             Force_old=Force_old+PK1(2,2,k1)
     !   enddo
 
       call Compute_Force(x,x_init,F,thichness,U,cor_W,disp,Force,Ci,count_hole,count_section,vol,nabla_W_0_1,nabla_W_0_2,N,mu0,k0,table,index_hole,index_section,pi,time_calculated)
        
        write (2,1112)  Force,x(2,index_hole(1))-x_init(2,index_hole(1))
    
        write (*,1112) Force,x(2,1)
        xplot(1:2,1:N,coutfr)=x
        coutfr=coutfr+1
    end if
   ! do i=1,N
    !    if(abs(maxPK133)<abs(Cauchy(3,3,i))) then
    !        maxPK133=Cauchy(3,3,i)
     !   end if
   ! enddo
  
             
  
   
   
    
enddo
    
do i=1,N
        write (3, 1111) x(1,i),x(2,i),s(i) !read coordinats
enddo
do i=1,N
        write (4, 1111) x(1,i),x(2,i),phi(i)-1.0d0 !read coordinats
    enddo
!конец блок интегрирования по времени 
  !  call surf(s,N)
    pause
    
    !гиф анимация процесса
    call  plot(xplot,N,50)
    
    
    deallocate(x)
    deallocate(x_init)
    deallocate(xplot)
    deallocate(v)
    deallocate(table)
    
    deallocate(acc)

    deallocate(s)
    deallocate(s_new)
    deallocate(YieldStress)
    deallocate(index_hole)
    deallocate(index_section) 
    
    deallocate(nabla_W_0_1)
    deallocate(nabla_W_0_2)
    deallocate(F)
    deallocate(Ci)
    deallocate(Ci_new)
    deallocate(Cauchy)
    deallocate(PK1)
    deallocate(U)

    
1100 format (10f10.6,1i5)
1110 format (1i12,1f24.0,1f21.0)
1111 format (3f13.6)
1112 format (2f25.6)
    
end program base
    
    
   
    
  
    
    
    
