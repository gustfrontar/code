
clear all
close all

%La idea de este script es calcular trayectorias de objetos, esto despues
%se puede usar para hacer una verificacion 4D.


%load('../DATA/Reanalisis/anomalia.mat');
%load('../WRF/anomalia.mat');
load('../anomalia3.mat');
anomalia500=flipdim(anomalia500,1);
anomalia=anomalia500;
anomalia_filt=anomalia500;
 for i=1:length(tiempos)
     hgt(i,:,:)=anomalia500(:,:,i);
 end
 
 %hgt=anomalia500;
 tiempos=tiempos;

%nx_s=61;
%ny_s=46;
nx_s=size(anomalia,2);
ny_s=size(anomalia,1);

%WRF GRID.
lat_s=-60:2.5:-10;
lon_s=270:2.5:320;
% lat_s=-90:2.5:90;
% lon_s=0:2.5:360-2.5;



[lon lat]=meshgrid(lon_s,lat_s);

% Elementos que deben estar presentes en la estructura input...
% Data: un array tridimensional con anomalias estandarizadas y filtradas.
% Data_unfilt: un array cuatridimensional (lat,lon,tiempo,variables) donde 
% esta el valor de las variables que van a ser promediadas y donde se
% buscaran los extremos para cada sistema. Aca irian los campos no
% filtrados de las variables.
% lat: Un array 2D con las latitudes.
% lon: Un array 2D con las longitudes.
% times: Un vector con las fechas (en formato numero de matlab).

input.Data=anomalia_filt;
input.Data_unfilt=anomalia;
input.lat=lat;
input.lon=lon;
input.times=tiempos;

config=[]; %Uso la configuracion por defecto.

[trayectorias]=calc_trayectory_fun(input,config);

figure
for i=1:length(trayectorias)
   
   hold on
   plot(trayectorias(i).lon,trayectorias(i).lat,'o-')
   axis([min(min(lon)) max(max(lon)) min(min(lat)) max(max(lat))])
   
end
print('-dpng',['./trayectorias.png']);
close(1);


mascara=trayectorias.mask;

for i=2:size(hgt,1)-1
   figure
   pcolor(lon,lat,mascara(:,:,i));
   shading flat
   hold on
   contour(lon,lat,squeeze(anomalia(:,:,i)),'k');
   caxis([0 30])

   for it=1:length(trayectorias)
   index=trayectorias(it).date_sis >= tiempos(i-1) & trayectorias(it).date_sis <= tiempos(i+1);
   lataux=trayectorias(it).lat(index);
   lonaux=trayectorias(it).lon(index);
   plot(lonaux,lataux,'ro-')
   axis([min(min(lon)) max(max(lon)) min(min(lat)) max(max(lat))])
   
   end
   print('-dpng',['./mascara' num2str(i) '.png']);
   close(1)
end

for i=1:size(hgt,1)
    
   auxiliar=(anomalia_filt(:,:,i) <= -50);
   mask=double(auxiliar);
   auxiliar=(anomalia_filt(:,:,i) <= -60);
   mask=mask+double(auxiliar);
   figure
   pcolor(mask);
   shading flat
   hold on
   contour(squeeze(anomalia(:,:,i)),'k');
   caxis([0 2])
   
   print('-dpng',['./anomalia' num2str(i) '.png']);
    close(1)
end



