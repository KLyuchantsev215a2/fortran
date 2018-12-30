!данный скрипт создает подготовленный файл с вхводными данными для метода
!полная инициализация начальный данных, работа с геометрией области
program geometry
!d размерность задачи нигде пока не используется
    implicit none 
    integer N,sqn,d
    integer i,yi,xi
    real rho_0, v_0, T, l
    real S,m,h
    real nu,mu 
    real dh
    real, allocatable :: x(:,:)
    
    open (unit=1, file="input.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
    open (unit=2, file="output.txt")
    
    read (1, 110) rho_0, v_0, T, l, sqn, d 
    write (*, 110) rho_0, v_0, T, l, sqn, d

    
    N=sqn*sqn
    S=l*l
    m=rho_0*S/N
    h=sqrt(1.4*(m/rho_0))
    
    

        allocate(x(2,N))
        i=1
        do yi=1,sqn
            do xi=1,sqn
                x(1,i) = real(xi)/real(sqn)*l
                x(2,i) = real(yi)/real(sqn)*l
                i=i+1
            enddo
        enddo
        
        do i=1,N
            write (2,111) x(1,i),x(2,i)
        enddo
        
        deallocate(x)
        pause
        
110 format (4f10.0,1i4,1i1)
111 format (2f)
    
end program geometry               