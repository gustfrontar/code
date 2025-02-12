clear all
close all

%Evaluation of bias in the forecast.


load model_data.mat
load lluvia_obs_hires.mat
load lat_lon.mat


nforecast=size(lluvia_obs,3);
ndays=size(lluvia_obs,4);

%Compare PDF.

bins=[1 5 10:10:100];  %Thresholds to compute ETS.

nbins=length(bins);
pdf_model=zeros(length(bins)-1,nforecast);
pdf_obs=pdf_model;

%Compute ETS.

for ii=1:nforecast
    tmp_model=reshape(lluvia_modelo(:,:,ii,:),[size(lluvia_modelo,1)*size(lluvia_modelo,2)*size(lluvia_modelo,4) 1]);
    tmp_obs=reshape(lluvia_obs(:,:,ii,:),[size(lluvia_obs,1)*size(lluvia_obs,2)*size(lluvia_obs,4) 1]);
    
    [ets(:,ii) hit(:,ii) far(:,ii)]=ets_fun(tmp_model,tmp_obs,bins);
    
    
end



%FIGURES:

%COMPARACION VISUAL DE 2 CAMPOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;
title('FORECAST')
pcolor(LON,LAT,lluvia_modelo(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)


subplot(1,2,2)
title('OBSERVATION')
pcolor(LON,LAT,lluvia_obs(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)

%Ploteamos los elementos de la tabla de contingencia.
umbral=10;
mascara_obs=real(lluvia_obs(:,:,6,7) > umbral);
mascara_mod=real(lluvia_modelo(:,:,6,7) > umbral);

figure
%subplot(2,2,3)
pcolor(LON,LAT,mascara_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-1 0.5 2],[2 28])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
contour(LON,LAT,mascara_mod,'k','LineWidth',2);
legend('FORECAST','OBSERVATION')
title(['PRECIPITATION GREATHER THAN ' num2str(umbral) ' mm']);

%Elementos de la tabla
mascara_tabla=NaN(size(mascara_obs));
index=(mascara_obs & mascara_mod);
mascara_tabla(index)=1;
index=(mascara_obs & ~mascara_mod);
mascara_tabla(index)=2;
index=(~mascara_obs & mascara_mod);
mascara_tabla(index)=3;
figure
%subplot(2,2,4)

pcolor(LON,LAT,mascara_tabla)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([ 0.5 1.5 2.5 3.5],[24 29 35])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('Contingency table components')



%Grafico los ets, pod y far
%==========================================================================

horas={'03h','06h','09h','12h','15h','18h','21h','24h','27h','30h','33h','36h','39h','42h','45h','48h'};
figure 
for ii=1:8
   for jj=1:2 
      index=(jj-1)*8+ii;
          subplot(2,8,index)
          hold on
          plot(bins,ets(:,index),'bo-','LineWidth',2)
          plot(bins,hit(:,index),'ro-','LineWidth',2)
          plot(bins,far(:,index),'go-','LineWidth',2)
          grid on;
          xlabel('Precipitation (mm)')
          ylabel(['Index ' horas{index}])
          axis([min(bins) max(bins) 0 1])
   end
end
legend('ETS','POD','FAR')

%GRAFICO LOS BIAS EN FRECUENCIA
%==========================================================================



%Grafico los ets, pod y far como funcion del plazo de pronostico
%==========================================================================

figure 



          subplot(2,1,1)
          hold on
          plot(3:3:48,ets(1,:),'bo-','LineWidth',2)
          plot(3:3:48,hit(1,:),'ro-','LineWidth',2)
          plot(3:3:48,far(1,:),'go-','LineWidth',2)
          grid on;
          xlabel('Time (hr)')
          ylabel('Index ')
          title(['Precipitation threshold ' num2str(bins(1))]);
          axis([3 48 0 1])
          
          subplot(2,1,2)
          hold on
          plot(3:3:48,ets(3,:),'bo-','LineWidth',2)
          plot(3:3:48,hit(3,:),'ro-','LineWidth',2)
          plot(3:3:48,far(3,:),'go-','LineWidth',2)
          grid on;
          xlabel('Time (hr)')
          ylabel('Index ')
          title(['Precipitation threshold ' num2str(bins(3))]);
          axis([3 48 0 1])


legend('ETS','POD','FAR')

%GRAFICO LOS BIAS EN FRECUENCIA
%==========================================================================









