% Calcula la precipitacion acumulada para el ciclo de las 12Z cada 24 hs y
% grafica el pronostico a 24, 48 y 72 horas.


clear all
close all



% Abro los archivo de los datos de hoy y me guardo la pp en una nueva
% variable
S=load('-ascii', 'fecha.txt');
fech=num2str(S);
fech=fech(1:8);
b=datenum(fech,'yyyymmdd');
bb=b+1;
bbb=b+2;
bbbb=b+3;
c=datevec(b);
cc=datevec(bb);
ccc=datevec(bbb);
cccc=datevec(bbbb);
tiempo_0=datestr(c,' dd mmmm 12Z'); % Fecha q se usa en los titulos de los graficos
tiempo_24=datestr(cc,' dd mmmm 12Z');
tiempo_48=datestr(ccc,' dd mmmm 12Z');
tiempo_72=datestr(cccc,' dd mmmm 12Z');
tie_0=datestr(c,'yyyymmdd');
tie_24=datestr(cc,'yyyymmdd');
tie_48=datestr(ccc,'yyyymmdd');
tie_72=datestr(cccc,'yyyymmdd');


% Leo los pronoticos de ese dia, son los pronosticos de la corrida de las
% 12Z
[fecha prono_24 prono_36 prono_48 prono_60 prono_72]=read_fun(fech,37,'/WRFV2/wrfsi/domains/operativo/archivo/3d/');
load lat_lon_wrf.mat

acumulada=prono_24+prono_48+prono_72;


latvar=lat;
lonvar=lon;


%***********************************GRAFICOS********************************
carga_mapa
v=[0 4 8 16 24 36 48 64 80 100 120];
%vcol=[2 31 41 43 45 47 55 57 59 27 ];
vcol=[2 55 53 45 42 32 21 24 27 29];



load coast


% Graficos para el pronostico a 24 horas
tas_24=isnan(prono_24(1,1));
if (tas_24~=1)
figure
pcolor(lonvar,latvar,prono_24)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', tiempo_24);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('wrfprono','24');
print('-depsc',archivo)
close 1
% else
%     nodisponible
%     archivo=strcat('wrfprono','24');
%     print('-depsc',archivo)
%     close 1
end



% Graficos para el pronostico a 48 horas
tas_48=isnan(prono_48(1,1));
if (tas_48~=1)
figure
pcolor(lonvar,latvar,prono_48)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', tiempo_48);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('wrfprono','48');
print('-depsc',archivo)
close 1
% else
%     nodisponible
%     archivo=strcat('wrfprono','48');
%     print('-depsc',archivo)
%     close 1
end


% Graficos para el pronostico a 72 horas
tas_72=isnan(prono_72(1,1));
if (tas_72~=1)
figure
pcolor(lonvar,latvar,prono_72)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', tiempo_72);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('wrfprono','72');
print('-depsc',archivo)
close 1
% else
%     nodisponible
%     archivo=strcat('wrfprono','72');
%     print('-depsc',archivo)
%     close 1
end



v_acum=[0 5 10 20 50 100 150 200 250 300 350];
vcol_acum=[2 55 53 45 42 32 21 24 27 29];

% Graficos de pp acumulada
tas_acum=isnan(acumulada(1,1));
if (tas_acum~=1)
figure
pcolor(lonvar,latvar,acumulada)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Precipitacion acumulada hasta el', tiempo_72);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v_acum,vcol_acum,0);
archivo=strcat('wrf','_acumulada');
print('-depsc',archivo)
close 1
% else
%     nodisponible
%     archivo=strcat('wrfprono','24');
%     print('-depsc',archivo)
%     close 1
end

