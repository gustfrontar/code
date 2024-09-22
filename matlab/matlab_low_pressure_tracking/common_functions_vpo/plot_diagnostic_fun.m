function []=plot_diagnostic_fun(group,pathfig)
%==========================================================================
% ESTA FUNCION PIDE UNA ESTRUCTURA GROUP CON TODOS LOS ERRORES CALCULADOS Y
% GENERA GRAFICOS DIAGNOSTICOS ACERCA DE LA EVOLUCION DEL ERROR EN LAS
% DISTINTAS VARIABLES.
%==========================================================================

enssize=size(group.minlat,1);
flength=size(group.minlat,2);


%==========================================================================
%ERROR Y SPREAD EN LA DISTANCIA TOTAL Y EN LA DISTANCIA EN LAT Y LON POR
%SEPARADO
%==========================================================================
%La distancia total
mask=( isnan(group.meandisterror) | isnan(group.ctrldisterror) | group.nummembers < enssize*0.9 );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.

figure
%plot(nansum(group.meandisterror.*group.nummembers,2)./nansum(group.nummembers,2),'k-','LineWidth',2);
tmp=group.meandisterror;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'k-','LineWidth',2);
hold on
tmp=group.ctrldisterror;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'k--','LineWidth',2);
%plot(nansum(group.distspread.*group.nummembers,2)./nansum(group.nummembers,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);
tmp=group.distspread;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Total distance error');legend('MEAN ERROR','CTRL ERROR','SPREAD');

%La distancia en latitud
figure
tmp=group.meandistlaterror;
tmp(mask)=NaN;
%plot(sqrt(nansum((group.meandistlaterror.^2).*group.nummembers,2)./nansum(group.nummembers,2)),'k-','LineWidth',2);
plot(sqrt(nanmean(tmp.^2,2)),'k-','LineWidth',2);
hold on
tmp=group.ctrldistlaterror;
tmp(mask)=NaN;
plot(sqrt(nanmean((tmp.^2),2)),'k--','LineWidth',2);
%plot(nansum(group.distlatspread.*group.nummembers,2)./nansum(group.nummembers,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);
tmp=group.distlatspread;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);
title('Lat distance error');legend('MEAN ERROR','CTRL ERROR','SPREAD');
%La distancia en longitud
figure
tmp=group.meandistlonerror;
tmp(mask)=NaN;
%plot(sqrt(nansum((group.meandistlonerror.^2).*group.nummembers,2)./nansum(group.nummembers,2)),'k-','LineWidth',2);
plot(sqrt(nanmean((tmp.^2),2)),'k-','LineWidth',2);
hold on
tmp=group.ctrldistlonerror;
tmp(mask)=NaN;
plot(sqrt(nanmean((tmp.^2),2)),'k--','LineWidth',2);
%plot(nansum(group.distlonspread.*group.nummembers,2)./nansum(group.nummembers,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);
tmp=group.distlonspread;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);
title('Lon distance error');legend('MEAN ERROR','CTRL ERROR','SPREAD');

%==========================================================================

%==========================================================================
% BIAS EN LA POSICION EN LONGITUD Y EN LA POSICION EN LATITUD.
%==========================================================================
mask=( isnan(group.meandisterror) | isnan(group.ctrldisterror));  %Esta mascara considera solo los puntos donde la media y el control estan presentes.

%Bias en longitud
figure
tmp=group.meandistlonerror;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'k-','LineWidth',2);
hold on
tmp=group.ctrldistlonerror;
tmp(mask)=NaN;
plot(nanmean(group.ctrldistlonerror,2),'k--','LineWidth',2);

title('Lon distance error');legend('MEAN ERROR','CTRL ERROR');


%Bias en latitude
figure
tmp=group.meandistlaterror;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'k-','LineWidth',2);
hold on
tmp=group.ctrldistlaterror;
tmp(mask)=NaN;
plot(nanmean(tmp,2),'k--','LineWidth',2);

title('Lon distance error');legend('MEAN ERROR','CTRL ERROR');

%==========================================================================
% PIT HISTOGRAM Y CALCULO DE DELTA.
%==========================================================================

mask=group.sistemexistance==0 | group.nummembers <  enssize*0.9;
tmppitlat=group.distlatpit;
tmppitlat(mask)=NaN;   %No calculamos el PIT si la cantidad de miembros es menor que 4.

tmppitlon=group.distlonpit;
tmppitlon(mask)=NaN;   %No calculamos el PIT si la cantidad de miembros es menor que 4.

iipit=1;
for iday=1:8:flength
edges=-0.05:0.1:1.05 %Defino los limites del intervalo.

tmp=histc(tmppitlat(iday,:),edges)';
tmp=tmp/sum(tmp);
pit_lat(iipit,:)=tmp(1:length(edges)-1); %El ultimo lugar del histograma no me interesa porque (ver help de la funcion histc).
%HISTC es una subrutina compilada y por ende maximiza la eficiencia.

deltalat(iipit)=mean((pit_lat(iipit,:)-1/size(pit_lat,2)).^2);

tmp=histc(tmppitlon(iday,:),edges)';
tmp=tmp/sum(tmp);
pit_lon(iipit,:)=tmp(1:length(edges)-1);tmppitlon=group.distlonpit;
tmppitlon(mask)=NaN;   %No calculamos el PIT si la cantidad de miembros es menor que 4.


deltalon(iipit)=mean((pit_lon(iipit,:)-1/size(pit_lon,2)).^2);

iipit=iipit+1;
end

figure
ntimes=size(pit_lat,1);
for ii=1:ntimes;
    x=(edges(1:end-1)+edges(2:end))/2;
    subplot(2,ntimes,ii)
    bar(x,pit_lat(ii,:));
    title('Lat distance')
    axis([0 1.05 0 0.5])
    subplot(2,ntimes,ii+ntimes)
    bar(x,pit_lon(ii,:));
    axis([0 1.05 0 0.5]) 
    title('Lon distance')
end

figure
hold on
plot(deltalon,'b-','LineWidth',2)
plot(deltalat,'r-','LineWidth',2)
legend('DELTALON','DELTALAT')


%==========================================================================
% SPREAD Y MEDIA DE LA REDUCED CENTERED RANDOM VARIABLE (RCRV)
%==========================================================================

mask=group.sistemexistance==0 | group.nummembers <  enssize*0.9 | group.distlonspread < 1e3 | group.distlatspread < 1e3;

tmp=group.rcvlon;
tmp(mask)=NaN;
rcvlonmean=nanmean(tmp,2);
rcvlonstd=nanstd(tmp,[],2);
tmp=group.rcvlat;
tmp(mask)=NaN;
rcvlatmean=nanmean(tmp,2);
rcvlatstd=nanstd(tmp,[],2);

figure
subplot(1,2,1)
plot(rcvlatmean,'k-','LineWidth',2);
hold on
plot(rcvlatstd,'k--','LineWidth',2);

subplot(1,2,2)
plot(rcvlonmean,'k-','LineWidth',2);
hold on
plot(rcvlonstd,'k--','LineWidth',2);


%==========================================================================
% RELIABILITY DIAGRAM PARA LA PREDICCION PROBABILISTICA DE LA OCURRENCIA
% DEL SISTEMA
%==========================================================================
%Compute probability.

mask=( group.sistemexistance == 0 & group.sistemprobability == 0);
tmpprob=group.sistemprobability;
tmpprob(mask)=NaN;
tmpexistance=double(group.sistemexistance);
tmpexistance(mask)=NaN;

%RELIABILITY DIAGRAM
for ii=1:size(tmpprob,1)
[reliability(ii,:),n_forecast(ii,:),prob_ref] = reliability_fun(tmpexistance(ii,:),tmpprob(ii,:),0.1);
end

a=jet(size(reliability,1));
icolor=1; 

figure
subplot(1,2,1)
for ii=1:4:size(reliability,1)
hold on
plot(prob_ref(2:end),reliability(ii,2:end)','LineWidth',2,'Color',a(ii,:))
end
legend('00','24','48','72','96','120','144','168');
plot([0 1],[0 1],'k--','LineWidth',2);

subplot(1,2,2)
for ii=1:4:size(reliability,1)
hold on
plot(prob_ref(2:end),n_forecast(ii,2:end)','LineWidth',2,'Color',a(ii,:))
end
legend('00','24','48','72','96','120','144','168');

%==========================================================================
% BSS AND ITS DECOMPOSITION
%==========================================================================

mask=( group.sistemexistance == 0 & group.sistemprobability == 0);
tmpprob=group.sistemprobability;
tmpprob(mask)=NaN;
tmpexistance=double(group.sistemexistance);
tmpexistance(mask)=NaN;

for ii=1:size(tmpprob,1)

[brier(ii) breli(ii) bresol(ii) bun(ii)]=brier_fun(tmpexistance(ii,:)',tmpprob(ii,:)',0.5,0.1);

end

figure
subplot(1,3,1)
plot(1-brier./bun,'LineWidth',2);
axis([0 29 -0.1 1])
title('BRIER SKILL SCORE')

subplot(1,3,2)
plot(breli,'LineWidth',2)
axis([0 29 0 0.3])
title('RELIABILITY')
subplot(1,3,3)
plot(bresol,'LineWidth',2)
axis([0 29 -0.3 0])
title('RESOLUTION')


%==========================================================================
% FRECUENCIA DE DETECCION DE LOS SISTEMAS POR PARTE DEL ANALYSIS Y POR
% PARTE DE LOS PRONOSTICOS.
%==========================================================================

mask=( group.sistemexistance == 0 & group.sistemprobability == 0);
tmpprob=group.sistemprobability;
tmpprob(mask)=NaN;
tmpexistance=double(group.sistemexistance);
tmpexistance(mask)=NaN;
tmpforexistance=double(tmpprob > 0);
tmpforexistance(mask)=NaN;

nsisforecast=nanmean(tmpforexistance,2);
nsisanalysis=nanmean(tmpexistance,2);
meanprob=nanmean(tmpprob,2);
%Cuantos sistemas tenemos en el pronostico y cuantos en el analysis.
figure
plot(nsisforecast,'k-','LineWidth',2);
hold on
plot(nsisanalysis,'k--','LineWidth',2);
plot(meanprob,'ko-','LineWidth',2);
title('SYSTEM FREQUENCY')
legend('FORECAST (P > 0)','ANALYSIS','MEAN PROBABILITY')

%Con que probabilidad en promedio se pronostica la existencia de un sistema
%a medida que avanza el plazo de pronostico.

%En cuantos sistemas hay un matching entre pronostico y observacion, esto
%es probabilidad mayor que 0 y observacion. 

%==========================================================================
% FIGURA CON LA DISTRIBUCION EN TIEMPO DE LA CANTIDAD DE SISTEMAS PRESENTES
% EN EL ANALISIS Y LA CANTIDAD DE SISTEMAS PRESENTES EN EL PRONOSTICO.
%==========================================================================

figure
plot(sum(group.sistemexistance,2),'k-','LineWidth',2);
hold on
plot(sum(group.sistemprobability,2),'k--','LineWidth',2);
xlabel('Forecast lead time')
ylabel('Number of trayectories')
legend('Analysis','Forecast')


%==========================================================================



