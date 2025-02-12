clear all; close all; clc

% ESTE SCRIPT GUARDA UN .MAT NO HACE GRAFICOS!!!

% Siguiendo el trabajo de ubicaci�n del Sol como interferencia en el radar
% de Ezeiza, este programa hace un campo medio de reflectividad para todo
% un d�a con los radares de INTA.

% PAR NO FUNCIONA




Path = '/ms-36/mrugna/tmp/';

load('mapas.mat')
clear samerica samerica2
load('amsur40.txt')

FileList = dir([Path, '*dBZ.vol']);

if isempty(FileList)==1;
    quit
end

i=3;
anio = FileList(i).name(1:4);
mes  = FileList(i).name(5:6);
dia  = FileList(i).name(7:8);

disp(length(FileList)) 
while i<length(FileList)+1
disp(i)

file = [Path FileList(i).name];
[radar] = rainbow5vol_orig(file);

[radar]=georeference_radar_data_orig(radar, []);
radar.data(radar.data==-32) = NaN;

if i==3;
elev1=radar.data(:,:,1);
else
elev1(:,:,2)=radar.data(:,:,1);

elev1=nanmean(elev1,3); %uso media para diario
end

elev=radar.elev(1);

i=i+1;
end

clear i

cantidad = length(FileList);
radar2.lon=radar.lon(:,:,1);
radar2.lat=radar.lat(:,:,1);
radar2.RadarName=radar.RadarName;
radar2.latitude=radar.latitude;
radar2.longitude=radar.longitude;

clear radar FileList

if strcmp(radar2.RadarName, 'INTA_Pergamino');
    filename = ['/ms-36/mrugna/salidas/PER_',anio,mes,dia,'.mat'];
elseif strcmp(radar2.RadarName, 'INTA_Anguil');
    filename = ['/ms-36/mrugna/salidas/ANG_',anio,mes,dia,'.mat'];
end
save(filename)

% Ahora se grafica y guarda la figura


% figure('visible','off')
% hold all
% if strcmp(radar2.RadarName, 'INTA_Pergamino');
%     pcolor(radar2.lon(1:360,:,1),radar2.lat(1:360,:,1),elev1)
% elseif strcmp(radar2.RadarName,'INTA_Anguil');
%     pcolor(radar2.lon(1:361,:,1),radar2.lat(1:361,:,1),elev1)
% end
% shading flat
% axis square tight
% caxis([-32 70])
% colorbar
% ylabel(colorbar,'Reflectividad (dBZ)')
% plot(radar2.lon(:,480),radar2.lat(:,480),'-k');
% if strcmp(radar2.RadarName, 'INTA_Pergamino');
%     plot([-64 -58],[radar2.latitude radar2.latitude],'-k');
%     plot([radar2.longitude radar2.longitude],[-37 -31],'-k');
% elseif strcmp(radar2.RadarName,'INTA_Anguil');
%     plot([-67 -61],[radar2.latitude radar2.latitude],'-k');
%     plot([radar2.longitude radar2.longitude],[-39 -34],'-k');
% 
% end
% box on
% 
% plot(municipios(:,2),municipios(:,1),'-','LineWidth',0.5,'Color',[0 0 0]);
% plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0 0 0]);
% % plot(amsur40(:,1),amsur40(:,2),'-','LineWidth',1,'Color','k');
% 
% 
% if strcmp(radar2.RadarName, 'INTA_Pergamino');
%     title({['Radar Pergamino  - Reflectividad media del ' ...
%         dia '/' mes '/' anio];['Elevacion: ' num2str(elev) 'deg n=' num2str(cantidad-2)]}, ...
%         'FontSize',14)
% elseif strcmp(radar.RadarName,'INTA_Anguil');
%     title({['Radar Anguil  - Reflectividad media del ' ...
%         dia '/' mes '/' anio];['Elevacion: ' num2str(elev) 'deg n=' num2str(cantidad-2)]}, ...
%         'FontSize',14)
% end
% 
% print('-dpng','-r150',[Path,'meanref_PER_',anio, mes, dia,'.png'])
% % Resoluci�n 150 ppi para subir a Sol1
% 
% clear radar FileList
% 
% % j = j+1;
% % end
% 
% clear i j



% figure
% hold all
% r = (0:0.5:239.5)';
% theta = pi*(0:360)/180;
% X = r*cos(theta);
% Y = r*sin(theta);
% pcolor(X,Y,elev1')
% shading flat
% axis equal tight
% caxis([-32 70])
% colorbar
% ylabel(colorbar,'Reflectividad (dBZ)')
% plot([-240 240],[0 0],'-k') % grafico 4 l�neas para referenciar �ngulos
% plot([0 0],[-240 240],'-k')
% plot([-240 240],[-240 240],'-k')
% plot([240 -240],[-240 240],'-k')
% plot([-240 240],[239 239],'-k') % grafico los 2 lados que faltan
% plot([239.5 239.5],[-240 240],'-k')
% plot(240*sin(2*pi*theta),240*cos(2*pi*theta),'-k');

%%% VERANO
% plot([0 236.3],[0 -41.7],'--b','LineWidth',2) % radiales del sol 
% plot([0 207.8],[0 -120],'--b','LineWidth',2) % radiales del sol 
% plot([0 -236.3],[0 -41.7],'--b','LineWidth',2) % radiales del sol 
% plot([0 -207.8],[0 -120],'--b','LineWidth',2) % radiales del sol 

%%% INVIERNO
% plot([0 236.3],[0 41.7],'--b','LineWidth',2) % radiales del sol 
% plot([0 207.8],[0 120],'--b','LineWidth',2) % radiales del sol 
% plot([0 -236.3],[0 41.7],'--b','LineWidth',2) % radiales del sol 
% plot([0 -207.8],[0 120],'--b','LineWidth',2) % radiales del sol 
