clear all
close all

datos=load('-ascii', 'estac.txt');

carga_mapa

latvar=datos(:,2);
lonvar=datos(:,3);

figure
plot(lonvar,latvar,'LineStyle','none','Marker','o','MarkerSize',7,'MarkerEdgeColor',[1 1 1],'MarkerFaceColor',[1 0 0])
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
axis([-85 -45 -60 -20])

figure
plot(lonvar,latvar,'LineStyle','none','Marker','o','MarkerSize',7,'MarkerEdgeColor',[1 1 1],'MarkerFaceColor',[1 0 0])
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
axis([-85 -35 -25 15])

figure
plot(lonvar,latvar,'LineStyle','none','Marker','o','MarkerSize',7,'MarkerEdgeColor',[1 1 1],'MarkerFaceColor',[1 0 0])
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
axis([-70 -50 -40 -20])
