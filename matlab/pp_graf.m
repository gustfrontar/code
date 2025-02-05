% calcula la diferencia entre el pronostico a 48 y el pronostico a 24 y
% luego promedia sobre los ultimos 15 dias. Hace el grafico de contornos.
% se le puede cambiar los valores de la escala.
% grafica tamb el ultimo dia de los datos Cmorph y los ultimos 15 dias de
% los Cmorph. Si los datos no estan para ese dia no grafica nada.
clear all
close all



% Llama a la pp acumulada del wrf y a las latitudes y longitudes del wrf
load pp_wrf.mat
load lat_lon_wrf.mat


% Caluclo la acumulada de los ultimos 15 dias y calculo la diferencia entre
% el prono a 24hs y el de 48 hs
a=size(pp_acum_wrf);
tmax=a(4);
tmin=a(4)-15;
diff=nanmean(pp_acum_wrf(:,:,3,tmin:tmax)-pp_acum_wrf(:,:,1,tmin:tmax),4);

% Calculo la acumulada de los ultimos 15 dias
diff_24=nanmean(pp_acum_wrf(:,:,1,tmin:tmax),4)*15;
diff_48=nanmean(pp_acum_wrf(:,:,3,tmin:tmax),4)*15;
diff_72=nanmean(pp_acum_wrf(:,:,5,tmin:tmax),4)*15;


% Cargo la fecha con q voy a leer los datos gts y los cmorph, ya q estan
% retrasados 5 dias.
S=load('-ascii', 'fech_graf.txt');
fech=num2str(S);
fech_gts=str2num(fech(1:8));
anio=str2num(fech(1:4));
mes=str2num(fech(5:6));
diaa=str2num(fech(7:8));

% Escribo la fecha de forma tal de q pueda qdar como titulo de los graficos
fech_rec=fech(1:8);
b=datenum(fech_rec,'yyyymmdd');
c=datevec(b);
tiempo_0=datestr(c,' dd mmmm 12Z'); % Fecha q se usa en los titulos de los graficos


%Genera la fecha q va a aparecer en el grafico
p=fecha_archivo';
aaa=size(p);
aa=aaa(1);
e=aaa(1)-15;
f=p(aa,:);
ii=p(e,:);
jj=[f 0 0 0];
n=[ii 0 0 0];
tempquince=datestr(n, ' dd mmmm'); %genera la fecha de hoy menos 15 dias
tempuno=datestr(jj, ' dd mmmm'); %genera la fecha de hoy menos 1 dia


% Genera el mapa para el WRF y grafica en contornos
carga_mapa
escala=[-7.5 -5 -3 -1 1 3 5 7.5];
color=[48 44 41 2 21 23 25];

latvar=lat;
lonvar=lon;
load coast

pcolor(lonvar,latvar,diff)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
title(strcat('Media de la acumulada desde',' ', tempquince, 'al','', tempuno, 'entre el pronostico a 48 y el pronostico a 24'),'FontSize',13)
[colores] = plot_jrcol(escala,color,0);
archivo=strcat('pp','media')
print('-depsc',archivo)
close 1;



figure
escala_cmorph=[0 5 30 60 100 150 200 300 400 500];
color_cmorph=[2 31 41 43 45 47 56 59 27]; 

pcolor(lonvar,latvar,diff_24)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tite=strcat('Precipitacion WRF acumulada ',' Desde ',tempquince,' al ',tiempo_0);
title(tite,'FontSize',13)
[colores] = plot_jrcol(escala_cmorph,color_cmorph,0);
archivo=strcat('pp','wrf_acum','_24')
print('-depsc',archivo)
close 1;


figure
escala_cmorph=[0 5 30 60 100 150 200 300 400 500];
color_cmorph=[2 31 41 43 45 47 56 59 27]; 

pcolor(lonvar,latvar,diff_48)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tite=strcat('Precipitacion WRF acumulada ',' Desde ',tempquince,' al ',tiempo_0);
title(tite,'FontSize',13)
[colores] = plot_jrcol(escala_cmorph,color_cmorph,0);
archivo=strcat('pp','wrf_acum','_48')
print('-depsc',archivo)
close 1;



figure
escala_cmorph=[0 5 30 60 100 150 200 300 400 500];
color_cmorph=[2 31 41 43 45 47 56 59 27]; 

pcolor(lonvar,latvar,diff_72)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tite=strcat('Precipitacion WRF acumulada ',' Desde ',tempquince,' al ',tiempo_0);
title(tite,'FontSize',13)
[colores] = plot_jrcol(escala_cmorph,color_cmorph,0);
archivo=strcat('pp','wrf_acum','_72')
print('-depsc',archivo)
close 1;



%********************************CMORPH*******************************
% abro los archivos donde estan los datos cmorph
load pp_cmorph.mat
t_cmorph=find(fecha_archivo(1,:)==anio & fecha_archivo(2,:)==mes & fecha_archivo(3,:)==diaa);
saber=isempty(t_cmorph);
tmax_cmorph=size(pp_acum_cmorph,3);
tmin_cmorph=size(pp_acum_cmorph,3)-15;

% calculo la acumulada de los ultimos 15 dias
cmorph=(nanmean(pp_acum_cmorph(:,:,tmin_cmorph:tmax_cmorph),3))*15;

% Le cambio los nan por ceros a la pp acumulada
jj=find(isnan(cmorph)==1);
cmorph(jj)=0;

% cambio los nan por ceros a la pp del dia
pp=pp_acum_cmorph(:,:,t_cmorph);
ii=find(isnan(pp)==1);
pp(ii)=0;

% grafico los acumulada de los ultimos 15 dias
figure
escala_cmorph=[0 5 30 60 100 150 200 300 400 500];
color_cmorph=[2 31 41 43 45 47 56 59 27]; 

pcolor(lonvar,latvar,cmorph)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tite=strcat('Precipitacion CMORPH acumulada ',' Desde ',tempquince,' al ',tiempo_0);
title(tite,'FontSize',13)
[colores] = plot_jrcol(escala_cmorph,color_cmorph,0);
archivo=strcat('pp','cmorph_acum')
print('-depsc',archivo)
close 1;


if (saber~=1)
% grafico la precipitacion del ultimo dia del archivo 
figure
esc_cmorph=[0 4 8 16 24 36 48 64 80 100 120];
col_cmorph=[2 55 53 45 42 32 21 24 27 29]; 

pcolor(lonvar,latvar,pp)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Precipitacion CMORPH valida para el ',tiempo_0);
title(tit,'FontSize',13)
[colores] = plot_jrcol(esc_cmorph,col_cmorph,0);
archivo=strcat('pp','cmorph');
print('-depsc',archivo)
close 1;
end


%****************************Datos GTS*************************************
% abro los archivos donde estan los datos gts
load pp_est.mat
t_gts=find(pp_acum_est(:,1)==fech_gts);
a=isempty(t_gts);



if (a~=1)
precip_est=pp_acum_est(t_gts,2:4);

lat_est=precip_est(:,1);
lon_est=precip_est(:,2);


% Me defino una reticula de latitud y longitud para interpolar los datos de
% las estaciones del superensamble
%N=20
N=105;
latitud(1,1)=-5;
longitud(1,1)=-85;

for i=0:N
   for j=0:N
       longitud(i+1,j+1)=longitud(1,1)+(0.5*j);
   end
end

for j=0:N
   for i=0:N
       latitud(i+1,j+1)=latitud(1,1)-(0.5*i);
   end
end


% Interpolo los datos de las estaciones a la reticula q me arme
% anteriormente
preci(:,:)=griddata(precip_est(:,2),precip_est(:,1),precip_est(:,3),longitud,latitud);

% Cambio los NaN por cero para poder graficar
faltan=(isnan(preci)==1);
preci(faltan)=0;

% Renombro la lattitud y la longitud para no q no me sobre-escriba la
% matriz
lon_var=longitud;
lat_var=latitud;

carga_mapa
%v=[0 0.5 1 1.5 2 2.5 3 3.5 4 4.5];
v=[0 4 8 16 24 36 48 64 80 100 120];
%vcol=[2 31 41 43 45 47 55 57 59 27 ];
vcol=[2 55 53 45 42 32 21 24 27 29];


load coast

figure
pcolor(lon_var,lat_var,preci)
shading flat
hold on
plot(lon_est,lat_est,'LineStyle','none','Marker','o','MarkerSize',5,'MarkerEdgeColor',[0 0 0],'MarkerFaceColor',[1 1 1])
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
%plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Precipitacion GTS valida para el ', tiempo_0);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('ppgts','_day');
print('-depsc',archivo)
close 1

end




