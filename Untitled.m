clear;   
F150=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\h_0_2\150\Force_SPH.txt');
F600=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\h_0_2\600\Force_SPH.txt');
F30=load('C:\Users\mmfhanter\Documents\GitHub\fortran\Отчет\Растяжение\ang\30\нелокальная\Force_SPH.txt');
F9600=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\h_0_2\9600\Force_SPH.txt');
F150s=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\150\Force_SPH.txt');
F600s=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\600\Force_SPH.txt');
F2400s=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\2400\Force_SPH.txt');
F9600s=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Нелокальная модель\9600\Force_SPH.txt');
F150s1=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Локальная модель\150\Force_SPH.txt');
F600s1=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Локальная модель\600\Force_SPH.txt');
F2400s1=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Локальная модель\2400\Force_SPH.txt');
F9600s1=load('C:\Users\mmfhanter\Desktop\Отчет\Растяжение\Локальная модель\9600\Force_SPH.txt');
Ftr=load('C:\Users\mmfhanter\Documents\GitHub\fortran\sphv3_трещина\Force_SPH.txt');
%F150=load('C:\Users\mmfhanter\Desktop\Отчет\Трещина\Локальная модель\Force_SPH.txt');
%plot(F9600s1(:,2),F9600s1(:,1),'.',F2400s1(:,2),F2400s1(:,1)*1.02,'.',F600s1(:,2),F600s1(:,1)*1.04,'.',F150s1(:,2),F150s1(:,1)*1.06,'.',F9600(:,2),F9600(:,1),F2400(:,2),F2400(:,1)*1.02,F600(:,2),F600(:,1)*1.04,F150(:,2),F150(:,1)*1.06,F9600s(:,2),F9600s(:,1),'o',F2400s(:,2),F2400s(:,1)*1.02,'o',F600s(:,2),F600s(:,1)*1.04,'o',F150s(:,2),F150s(:,1)*1.06,'o');
%plot(F30(:,2),F30(:,1),'.',F600(:,2),F600(:,1)*1.02);
%plot(Ftr(:,2),Ftr(:,1));
surfs600=load('C:\Users\mmfhanter\Documents\GitHub\fortran\sphv3_повреждаемость\s.txt');
surfz=zeros(600,1);
T = delaunay(surfs600(:,1),surfs600(:,2));
TO = triangulation(T,surfs600(:,1),surfs600(:,2),surfs600(:,3));
trisurf(TO);
%  for j=1:20
   %      for i=1:30
    %         l=1+20*(i-1);

   %         z(i,j)=surfz(l+j-1);

   %                  end
  %end
    

%plot3(surfs600(:,1), surfs600(:,2), surfs600(:,3),'.');