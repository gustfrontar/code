function plot_forecasts(OBS,FORECAST,FECHAS,FIG_PATH,ESTACION)

%ESTA FUNCION GRAFICA LA SERIE COMPLETA DE OBSERVACIONES Y PRONOSTICOS SIN
%DISCRIMIAR POR HORA DEL DIA.
%==========================================================================
% Juan Ruiz (2009)

[ntiempos nhoras nforecast nvars]=size(FORECAST);

fecha{1}=datestr(datenum(num2str(FECHAS(1)),'yyyymmddHH'),'yy/mm/dd');
fecha{3}=datestr(datenum(num2str(FECHAS(end)),'yyyymmddHH'),'yy/mm/dd');
medio=round(length(FECHAS)/2);
fecha{2}=datestr(datenum(num2str(FECHAS(medio)),'yyyymmddHH'),'yy/mm/dd');

%Obtenemos la serie completa a partir de las observaciones.
OBS_serie=NaN(length(FECHAS)*nhoras,nvars);
for i=1:nhoras
OBS_serie(i:nhoras:end-nhoras+i,:)=OBS(:,i,:);
end

%Obtengo las series completas para los pronsoticos.
count=0;
npronosticos=length(FECHAS)*2+5;  %Cuantos pronosticos tiene valores para el rango de fechas especificado.
FORECAST_serie=NaN(npronosticos,length(FECHAS)*nhoras,nvars);
for i=1:ntiempos
for ihour=1:nhoras
    if(ihour==3 | ihour==1);count=count+1;end
    for ifor=1:5
FORECAST_serie(6-ifor+count,(i-1)*4+ihour,:)=FORECAST(i,ihour,ifor,:);
    end
end
end


%Definimos la escala de colores a usar.
jrcol(1,:)=[ 255 192  60];
jrcol(2,:)=[ 250 100  30];
jrcol(3,:)=[ 225  20   0];
jrcol(4,:)=[ 180 250 170];
jrcol(5,:)=[  55 210  60];
jrcol(6,:)=[ 130 210 255];
jrcol(7,:)=[  40 150 210];
jrcol(8,:)=[ 160 140 255];
jrcol(9,:)=[  60  40 180];
jrcol(10,:)=[ 248 160 160];
jrcol=jrcol/255;

figure
subplot(2,1,1)
hold on
plot(OBS_serie(:,1),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);
legend('SYNOP')
  
top1=max(OBS_serie(:,1))+5;
bot1=min(OBS_serie(:,1))-5;
top2=max(max(FORECAST_serie(:,:,1)))+5;
bot2=min(min(FORECAST_serie(:,:,1)))-5;
top=max([top1 top2]);
bot=min([bot1 bot2]);

if(isnan(top));top=5;end
if(isnan(bot));bot=-5;end

color=1;
for iprono=1:npronosticos 
   plot(FORECAST_serie(iprono,:,1),'Color',jrcol(color,:),'LineWidth',2); 
   color=color+1;
   if(color > 10);color=1;end
end
%Vuelvo a plotear las observaciones para que queden en el tope.
plot(OBS_serie(:,1),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);


title(strcat('Temperatura, ESTACION=',ESTACION));
xlabel('fecha');ylabel('Temperatura');
set(gca,'XGrid','On','YGrid','On');
set(gca,'XTick',[1 medio*nhoras length(FECHAS)*nhoras],'XTickLabel',{fecha{1};fecha{2};fecha{3}});
axis([1 length(FECHAS)*nhoras bot top]);

subplot(2,1,2)
hold on
plot(OBS_serie(:,2),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);
legend('SYNOP')
  
top1=max(OBS_serie(:,2))+5;
bot1=min(OBS_serie(:,2))-5;
top2=max(max(FORECAST_serie(:,:,2)))+5;
bot2=min(min(FORECAST_serie(:,:,2)))-5;
top=max([top1 top2]);
bot=min([bot1 bot2]);

if(isnan(top));top=5;end
if(isnan(bot));bot=-5;end

color=1;
for iprono=1:npronosticos 
   plot(FORECAST_serie(iprono,:,2),'Color',jrcol(color,:),'LineWidth',2); 
   color=color+1;
   if(color > 10);color=1;end
end

%Vuelvo a plotear las observaciones para que queden en el tope.
plot(OBS_serie(:,2),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);

title(strcat('Td, ESTACION=',ESTACION));
xlabel('fecha');ylabel('Temperatura');
set(gca,'XGrid','On','YGrid','On');
set(gca,'XTick',[1 medio*nhoras length(FECHAS)*nhoras],'XTickLabel',{fecha{1};fecha{2};fecha{3}});
axis([1 length(FECHAS)*nhoras bot top]);



pngname=strcat(FIG_PATH,ESTACION,'_MARCHADIURNA.png');
print('-dpng',pngname);

close all


figure
subplot(2,1,1)
hold on
plot(OBS_serie(:,6),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);
legend('SYNOP')

top1=max(OBS_serie(:,6))+5;
bot1=min(OBS_serie(:,6))-5;
top2=max(max(FORECAST_serie(:,:,6)))+5;
bot2=min(min(FORECAST_serie(:,:,6)))-5;
top=max([top1 top2]);
bot=min([bot1 bot2]);

if(isnan(top));top=5;end
if(isnan(bot));bot=-5;end

color=1;
for iprono=1:npronosticos
   plot(FORECAST_serie(iprono,:,6),'Color',jrcol(color,:),'LineWidth',2);
   color=color+1;
   if(color > 10);color=1;end
end
%Vuelvo a plotear las observaciones para que queden en el tope.
plot(OBS_serie(:,6),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);


title(strcat('Velocidad, ESTACION=',ESTACION));
xlabel('fecha');ylabel('Velocidad');
set(gca,'XGrid','On','YGrid','On');
set(gca,'XTick',[1 medio*nhoras length(FECHAS)*nhoras],'XTickLabel',{fecha{1};fecha{2};fecha{3}});
axis([1 length(FECHAS)*nhoras bot top]);

subplot(2,1,2)
hold on
plot(OBS_serie(:,7),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);
legend('SYNOP')

top=360;
bot=0;

color=1;
for iprono=1:npronosticos
   plot(FORECAST_serie(iprono,:,7),'Color',jrcol(color,:),'LineWidth',2);
   color=color+1;
   if(color > 10);color=1;end
end

%Vuelvo a plotear las observaciones para que queden en el tope.
plot(OBS_serie(:,7),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);

title(strcat('Direccion, ESTACION=',ESTACION));
xlabel('fecha');ylabel('Direccion');
set(gca,'XGrid','On','YGrid','On');
set(gca,'XTick',[1 medio*nhoras length(FECHAS)*nhoras],'XTickLabel',{fecha{1};fecha{2};fecha{3}});
axis([1 length(FECHAS)*nhoras bot top]);



pngname=strcat(FIG_PATH,ESTACION,'_MARCHADIURNA_VIENTO.png');
print('-dpng',pngname);

close all
