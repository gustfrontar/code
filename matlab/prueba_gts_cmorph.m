% calcula la diferencia entre el pronostico a 48 y el pronostico a 24 y
% luego promedia sobre los ultimos 15 dias. Hace el grafico de contornos.
% se le puede cambiar los valores de la escala.
% grafica tamb el ultimo dia de los datos Cmorph y los ultimos 15 dias de
% los Cmorph
clear all
close all

% Llama a la pp acumulada del wrf y a las latitudes y longitudes del wrf
load lat_lon_wrf.mat

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


% Genera el mapa para el WRF y grafica en contornos
carga_mapa

latvar=lat;
lonvar=lon;
load coast


%********************************CMORPH*******************************
% abro los archivos donde estan los datos cmorph
% load pp_cmorph.mat
% t_cmorph=find(fecha_archivo(1,:)==anio & fecha_archivo(2,:)==mes & fecha_archivo(3,:)==diaa);
% saber=isempty(t_cmorph)

% cambio los nan por ceros a la pp del dia
% pp=pp_acum_cmorph(:,:,t_cmorph);
% ii=find(isnan(pp)==1);
% pp(ii)=0;

% if (saber~=1)
% % grafico la precipitacion del ultimo dia del archivo 
% figure
% esc_cmorph=[0 4 8 16 24 36 48 64 80 100 120];
% col_cmorph=[2 55 53 45 42 32 21 24 27 29]; 
% 
% pcolor(lonvar,latvar,pp)
% shading flat
% hold on
% plot(lon_costa,lat_costa,'k')
% plot(lon_pais,lat_pais,'k')
% plot(lon_rios,lat_rios,'b')
% plot(lon_lagos,lat_lagos,'b')
% plot(lon_ciudad,lat_ciudad,'ro')
% tit=strcat('Precipitacion CMORPH valida para el ',tiempo_0);
% title(tit,'FontSize',13)
% [colores] = plot_jrcol(esc_cmorph,col_cmorph,0);
% archivo=strcat('pp','cmorph');
% % print('-depsc',archivo)
% % close 1;
% end


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
% print('-depsc',archivo)
% close 1

end
