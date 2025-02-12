clear all
close all

%Evaluation of bias in the forecast.


load model_data.mat
load lluvia_obs_hires.mat
load lat_lon.mat


nforecast=size(lluvia_obs,3);
ndays=size(lluvia_obs,4);

%Compare PDF.

bins=[-1 0 0.01 0.1 1 2.5 5 7.5 10:200];  %Bins to compute pdf.
cbins=(bins(1:end-1)+bins(2:end))/2;

nbins=length(bins);
pdf_model=zeros(length(bins)-1,nforecast);
pdf_obs=pdf_model;
%Compute PDF.

for ii=1:nforecast
     for jj=1:ndays
        for kk=1:nbins-1
            pdf_model(kk,ii)=pdf_model(kk,ii)+ nansum(nansum(lluvia_modelo(:,:,ii,jj) > bins(kk) & lluvia_modelo(:,:,ii,jj) <= bins(kk+1)));
            pdf_obs(kk,ii)=pdf_obs(kk,ii)+ nansum(nansum(lluvia_obs(:,:,ii,jj) > bins(kk) & lluvia_obs(:,:,ii,jj) <= bins(kk+1)));
        end
     end
     
     for kk=1:nbins
        frec_model(kk,ii)=nansum(nansum(nansum(lluvia_modelo(:,:,ii,:) > bins(kk) )));
        frec_obs(kk,ii)=nansum(nansum(nansum(lluvia_obs(:,:,ii,:)      > bins(kk) ))); 
     end
        media_model(ii)=nanmean(nanmean(nanmean(lluvia_modelo(:,:,ii,:))));
        media_obs(ii)=nanmean(nanmean(nanmean(lluvia_obs(:,:,ii,:))));
        bias_frec=frec_model./frec_obs;
        bias_mean=media_model./media_obs;
end

for ii=1:nforecast
   pdf_model(:,ii)=pdf_model(:,ii)/sum(pdf_model(:,ii)); 
   pdf_obs(:,ii)=pdf_obs(:,ii)/sum(pdf_obs(:,ii)); 
end


%FIGURES:

%COMPARACION VISUAL DE 2 CAMPOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,lluvia_modelo(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)

subplot(1,2,2)
pcolor(LON,LAT,lluvia_obs(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)

%Grafico las pdf
%==========================================================================

horas={'03h','06h','09h','12h','15h','18h','21h','24h','27h','30h','33h','36h','39h','42h','45h','48h'};
figure 
for ii=1:8
   for jj=1:2 
      index=(jj-1)*8+ii;
          subplot(2,8,index)
          hold on
          plot(cbins(2:end),log(pdf_model(2:end,index)),'b-','LineWidth',2)
          plot(cbins(2:end),log(pdf_obs(2:end,index)),'r-','LineWidth',2)
          grid on;
          set(gca,'YTick',[log(1e-6) log(1e-3) log(1e0)],'YTickLabel',{'1e-6','1e-3','1e0'});
          %set(gca,'XTick',cbins(2:5:end));
          xlabel('Precipitation (mm)')
          ylabel(['Prob. ' horas{index}])
          axis([min(cbins(2:end)) 150 log(1e-6) 0 ])
   end
end
legend('FORECAST','OBSERVATION')

%GRAFICO LOS BIAS EN FRECUENCIA
%==========================================================================


horas={'03h','06h','09h','12h','15h','18h','21h','24h','27h','30h','33h','36h','39h','42h','45h','48h'};
figure 
for ii=1:8
   for jj=1:2 
      index=(jj-1)*8+ii;
          subplot(2,8,index)
          hold on
          plot(bins(2:end),bias_frec(2:end,index),'b-','LineWidth',2)
          grid on;
          %set(gca,'XTick',cbins(2:10:end));
          xlabel('Precipitation (mm)')
          ylabel(['Frec. bias ' horas{index}])
          axis([min(cbins(2:end)) 150 0 6 ])
   end
end


%GRAFICO LOS BIAS EN LA MEDIA
%==========================================================================

figure 


hold on
plot(3:3:48,media_model,'bo-','LineWidth',2)
plot(3:3:48,media_obs,'ro-','LineWidth',2)
grid on;
          
xlabel('Time (hr)')
ylabel(['Mean precipitation'])
title('Mean precipitation')
legend('FORECAST','OBSERVATIONS');





