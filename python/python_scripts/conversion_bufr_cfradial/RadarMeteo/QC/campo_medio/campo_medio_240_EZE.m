clear all; close all; clc

% Siguiendo el trabajo de ubicación del Sol como interferencia en el radar
% de Ezeiza, este programa hace un campo medio de reflectividad para todo
% un día.

% ESTE SCRIPT NO GRAFICA!!! SOLO GUARDA LOS ARCHIVOS EN UN .MAT PARA 
% PODER GRAFICAR AFUERA

% Creo una lista con los archivos dentro del path
Path = '/ms-36/mrugna/tmp/';
List = dir([Path 'header.*']);

% Si alguna carpeta está vacía, que salga del programa para que no tire un
% error que haya que resolver manualmente
if isempty(List)==1;
    quit
else
    

load('mapas.mat')
clear samerica samerica2
load('amsur40.txt')

i=1;
anio = List(i).name(16:19);
mes  = List(i).name(20:21);
dia  = List(i).name(22:23);

disp(length(List)) 

% Para cada elemento de la lista genero el volumen del radar 
while i<length(List)+1
    disp(i)

    filehea    = [Path 'header.240km.z.' List(i).name(16:end)];
    filedat    = [Path 'volscan.240km.z.' List(i).name(16:end)];

    lat        = -34.7877;
    lon        = -58.5365;
    altura     = 30;   % m
    beam_wid_h = 0.98; % deg
    beam_wid_v = 0.98; % deg

    [radar data] = read_radar_rvd(filehea,filedat,lon,lat,altura,beam_wid_h,beam_wid_v);
    radar.REF=data;
    sounding.nada=[]; %Dummy argument setting.
    [radar]=georeference_radar_data_orig(radar,sounding);
    radar.REF(radar.REF==-32) = NaN;

    clear sounding filehea filedat altura beam_wid_h beam_wid_v data

    % Del volumen agarro la primera elevación y 1) si es el primer 
    % volumen lo dejo como esta o 2) para todos los demás, promedio con
    % los anteriores
    if i==1;
        elev1=radar.REF(:,:,1);
    else
        elev1(:,:,2)=radar.REF(:,:,1);

        elev1=nanmean(elev1,3);
    end

    elev=radar.elev(1);

    i=i+1;
end

clear i PathDat PathHea

% Si tengo algun problema con el graficado dentro del script, guardo las
% variables que me interesan en un archivo .mat

cantidad = length(List);
radar2.lon=radar.lon(:,:,1);
radar2.lat=radar.lat(:,:,1);

clear radar List
filename = ['/ms-36/mrugna/salidas/EZE_',anio,mes,dia,'.mat'];

save(filename)

% Ahora grafico la figura referenciada con un mapa y la guardo en ms-36


% figure('visible','off')
% hold all
% pcolor(radar.lon(:,:,1),radar.lat(:,:,1),elev1)
% shading flat
% axis square tight
% caxis([-32 70])
% colorbar
% ylabel(colorbar,'Reflectividad (dBZ)')
% % Algunas líneas, radio de 240 km y líneas de lat lon pasando por el radar
% plot(radar.lon(:,480),radar.lat(:,480),'-k');
% plot([-62 -55],[lat lat],'-k');
% plot([lon lon],[-37 -32],'-k');
% box on
% 
% plot(municipios(:,2),municipios(:,1),'-','LineWidth',0.5,'Color',[0 0 0]);
% plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0 0 0]);
% plot(amsur40(:,1),amsur40(:,2),'-','LineWidth',1,'Color','k');
% 
% title({['Radar Ezeiza  - Reflectividad media del ' ...
%     dia '/' mes '/' anio];['Elevación: ' num2str(elev) '° n=' num2str(length(List))]}, ...
%     'FontSize',13)
% 
% print('-dpng','-r150',[Path,'meanref_',anio, mes, dia,'.png'])
% Resolución 150 ppi para subir a Sol1

clear radar FileList


clear i j

end
