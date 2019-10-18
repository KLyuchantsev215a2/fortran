subroutine surf(s,Nn)
        use ogpf
        type(gpf):: gp
        integer :: Nn,i,j,l,k
         real*8 :: s(Nn)
        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z(:,:)
        integer:: m
        integer:: n
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! create the xyz data
        
        call meshgrid(x, y, linspace(-0.1_wp,1.2_wp, 20), linspace(-0.03_wp,1.53_wp, 30)  )

        n=size(x,1)
        m=size(x,2)
        allocate( z(n,m) )
       
         !z(1:20,1)=s(1:20)
         !z(1:20,2)=s(21:40)
         !z(1:20,3)=s(41:60)
         !z(1:20,4)=s(61:81)
         do j=1,20
         do i=1,30
             l=1+20*(i-1)
           !  k=20*(i-1)+20
             z(i,j)=s(l+j-1)
         enddo
         enddo
         
             
        call gp%options('unset key')
        call gp%options('unset surface')
        call gp%axis([real(wp):: -0.1, 1.2, -0.03, 1.53])

        !plot the contour
        call gp%title('Example 105: Contour plot')
        call gp%options('unset border')
        call gp%surf(x,y,z, palette='accent')
        call gp%contour(x,y,z, palette='jet')

    end subroutine surf