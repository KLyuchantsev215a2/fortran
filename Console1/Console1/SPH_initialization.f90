!������ ������ ������� �������������� ���� � ��������� ������� ��� ������
!������ ������������� ��������� ������, ������ � ���������� �������
program geometry
!d ����������� ������ ����� ���� �� ������������
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
    
    
110 format (4f10.0,1i4,1i1)
        allocate(x(2,N))
        i=1
        do yi=1,sqn
            do xi=1,sqn
                x(1,i) = real(xi)/real(sqn)*l
                x(2,i) = real(yi)/real(sqn)*l
                i=i+1
            enddo
        enddo
        write (2,*) x
        deallocate(x)
        pause
end program geometry               