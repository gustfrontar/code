function plot_estation(OBS,FORECAST,FECHAS,FIG_PATH,VARIABLE,ESTACION,LFORECAST,LHORA)

%ESTA FUNCION RECIBE LAS OBS Y LOS PRONOSTICOS PARA UNA ESTACION EN
%PARTICULAR Y GRAFICA UNA FIGURA CON 4 PANELES, UNA POR CADA HORA DEL DIA
%CON LA MARCHA OBSERVADA Y LA PRONOSTICADA A DIFERENTES PLAZOS.
%FIG_PATH es el directorio donde se generara la figura.
%==========================================================================
% Juan Ruiz (2009)

figure
[ntiempos nhoras nforecast]=size(FORECAST);

fecha{1}=datestr(datenum(num2str(FECHAS(1)),'yyyymmddHH'),'yy/mm/dd');
fecha{3}=datestr(datenum(num2str(FECHAS(end)),'yyyymmddHH'),'yy/mm/dd');
medio=round(length(FECHAS)/2);
fecha{2}=datestr(datenum(num2str(FECHAS(medio)),'yyyymmddHH'),'yy/mm/dd');


%Definimos la escala de colores a usar.
jrcol(1,:)=[ 255 250 170];
jrcol(2,:)=[ 255 232 120];
jrcol(3,:)=[ 255 192  60];
jrcol(4,:)=[ 255 160   0];
jrcol(5,:)=[ 255  96   0];
jrcol(6,:)=[ 255  50   0];
jrcol(7,:)=[ 225  20   0];
jrcol(8,:)=[ 192   0   0];
jrcol(9,:)=[ 165   0   0];
jrcol=jrcol/255;

for ihour=1:4
subplot(2,2,ihour);

hold on
  
top1=max(OBS(:,ihour))+5;
bot1=min(OBS(:,ihour))-5;
top2=max(FORECAST(:,ihour))+5;
bot2=min(FORECAST(:,ihour))-5;
top=max([top1 top2]);
bot=min([bot1 bot2]);

if(isnan(top));top=5;end
if(isnan(bot));bot=-5;end

for ifor=1:nforecast  
   plot(FORECAST(:,ihour,ifor),'Color',jrcol(ifor+2,:),'LineWidth',2); 
   text(ifor*2-1,top-5,LFORECAST{ihour,ifor},'Color',jrcol(ifor+2,:));
   text(1,top-2,'SYNOP','Color',[0 0 0]);
end

plot(OBS(:,ihour),'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',10);

title(strcat(VARIABLE,', HORA=',LHORA{ihour}));
xlabel('fecha');ylabel(VARIABLE);
set(gca,'XGrid','On','YGrid','On');
set(gca,'XTick',[1 medio length(FECHAS)],'XTickLabel',{fecha{1};fecha{2};fecha{3}});
axis([1 length(FECHAS) bot top]);

end


annotation('textbox',[0.42 0.001 0.16 0.05],'String',strcat('Estacion=',ESTACION))


pngname=strcat(FIG_PATH,ESTACION,'_',VARIABLE,'_MARCHA.png');
print('-dpng',pngname);

close all


