!������ ������ ������� �������������� ���� � ��������� ������� ��� ������
!������ ������������� ��������� ������, ������ � ���������� �������
    
1000 format (2f10.0)
real rho !��������� ���������
real v_0 !�������� ������� ����� ��������
    read (iin,1000) hed,numnp,(idof(i),i=1,6),negl,negnl,imass,idamp,
     1                modex,nste,dt,tstart,ieig,ipri,nskew
    open(1,FILE='input.txt')
    read(1,*) rho
    write(*,*) rho
    read(1,*) v_0
    write(*,*) v_0
    pause
end