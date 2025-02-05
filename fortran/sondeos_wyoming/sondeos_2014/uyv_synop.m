%Conversion de intensidad de viento y direccion en sus componentes u y v
clear all
close all

load /home/juan/Dropbox/Niebla/Scripts/datoseze.mat

%convierto las direcciones en angulos
tita2=NaN(1,281255);
u=NaN(1,281255);
v=NaN(1,281255);

tita= dirviento*10;


tita(tita==990)=360*rand(sum(tita==990),1);


tita=tita';

for i=1:281255
    u(i)=-viento(i)*sin(tita(i));
    v(i)=-viento(i)*cos(tita(i));
end

%Ahora los corto en 3 hs en formato ref2
fechainicial='1984120100';
fechafinal='2010122900'; %ver hasta que dia quedan 9527
inicio=datenum(fechainicial,'yyyymmddHH');
fin=datenum(fechafinal,'yyyymmddHH');

dia_inicio=find(tiempo==inicio);
dia_fin=find(tiempo==fin);



u_synop_3=NaN(9527,17);
v_synop_3=NaN(9527,17);

for i= dia_inicio:24:dia_fin;
    fila=(i-dia_inicio)/24+1; %cada dia
    tiempo2(fila)=tiempo(i);  
    v_synop_3(fila,:)=v(i:3:i+48);
    u_synop_3(fila,:)=u(i:3:i+48);
   
end 

save('uyvsynop_3_new.mat', 'u','v','u_synop_3','v_synop_3','tiempo2','tita','tita2')
