clear all
close all

% Abro las latitudes y longitudes del WRF
load lat_lon_wrf.mat

%Defino la reticula a la cual quiero interpolar q es la reticula del WRF 
for j=0:100
    for i=0:109
        lati(i+1,j+1)=lat(i+1);
        %lati(i+1,j+1)=lat(110-i);
    end
end
for i=0:109
    for j=0:100
        long(i+1,j+1)=lon(j+1);
    end
end

% Cargo la mascaras de la cuenca del plata
load mask_interpolada.mat
suma=mascara1*1+mascara2*2+mascara3*3+mascara4*4;


% Cargo las estaciones q estan dentro de la cuenca del Plata
datos=load('-ascii', 'est_cuenca.txt');
lat_est=datos(:,2);
lon_est=datos(:,3);

% Cargo las estaciones q estan sobre el dominio WRF
estaciones=load('-ascii', 'estac_acerca.txt'); 
lat_estaciones=estaciones(:,2);
lon_estaciones=estaciones(:,3);


% Renombro las lat y lon para poder graficar
lonvar=long;
latvar=lati;
carga_mapa
v=[0 1 2 3 4 5 6 7 8 9 10];
% %vcol=[2 31 41 43 45 47 55 57 59 27 ];
vcol=[2 55 53 45 42 32 21 24 27 29];
load coast


% Graficos 

figure
pcolor(lonvar,latvar,suma)
shading flat
hold on
plot(lon_est,lat_est,'LineStyle','none','Marker','o','MarkerSize',6,'MarkerEdgeColor',[0 0 1],'MarkerFaceColor',[1 0 0.2])
plot(lon_estaciones,lat_estaciones,'LineStyle','none','Marker','o','MarkerSize',4,'MarkerEdgeColor','none','MarkerFaceColor',[1 0 0.2])


plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
xlabel('Longitud','FontSize',12);
ylabel('Latitud','FontSize',12);
[colores] = plot_jrcol(v,vcol,1);
axis([-100 -30 -61 -3])
print('-dpng','mascara')


