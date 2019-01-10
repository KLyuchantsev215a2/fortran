!данный скрипт создает подготовленный файл с вхводными данными для метода
!полная инициализация начальный данных, работа с геометрией области
program geometry
!d размерность задачи нигде пока не используется
 
    integer N,sqn,d
    integer i,yi,xi
    real rho_0, v_0, T, l
    real S,m,h
    real nu,mu 
    real dh
    real, allocatable :: x(:,:)
    real, allocatable :: v(:,:)
    
    open (unit=1, file="input.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
    open (unit=2, file="output.txt")
    
    read (1, 1100) rho_0, v_0, T, l, sqn, d 
    write (*, 1100) rho_0, v_0, T, l, sqn, d

    
    N=sqn*sqn
    S=l*l
    m=rho_0*S/N
    h=sqrt(1.4*(m/rho_0))
    
    

        allocate(x(2,N))
        i=1
        do yi=1,sqn
            do xi=1,sqn
                x(1,i) = real(xi-1)/real(sqn-1)*l
                x(2,i) = real(yi-1)/real(sqn-1)*l
                i=i+1
            enddo
        enddo
        
        do i=1,N
            write (2,1110) i,x(1,i),x(2,i)
        enddo
       
        
        
        allocate(v(2,N))
        do i=1,N
            v(1,i)=x(2,i)*v_0
            v(2,i)=0
        enddo
        
        do i=1,N
            write (2,1110) i,v(1,i),v(2,i)
        enddo
        
        deallocate(x)
        deallocate(v)
        
        pause
        
1100 format (4f10.0,1i3,1i1)
1110 format (1i5,2f)
    
end program geometry               