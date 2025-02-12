function plot_estation(RMSE,RMSEDB,ECMSS,BIAS,FECHAS,FIG_PATH,VARIABLE,ESTACION,LFORECAST,LHORA)

%ESTA FUNCION RECIBE LAS OBS Y LOS PRONOSTICOS PARA UNA ESTACION EN
%PARTICULAR Y GRAFICA UNA FIGURA CON 4 PANELES, UNA POR CADA HORA DEL DIA
%CON LA MARCHA OBSERVADA Y LA PRONOSTICADA A DIFERENTES PLAZOS.
%FIG_PATH es el directorio donde se generara la figura.
%==========================================================================
% Juan Ruiz (2009)

figure
[nhoras nforecast]=size(RMSE);

fecha{1}=datestr(datenum(num2str(FECHAS(1)),'yyyymmddHH'),'yy/mm/dd');
fecha{2}=datestr(datenum(num2str(FECHAS(end)),'yyyymmddHH'),'yy/mm/dd');


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

 subplot(2,2,1)
   top=4;
   bar(RMSE); 
   axis([0.5 nhoras+0.5 0 top])
   colormap(jrcol);
   title(strcat(VARIABLE,', RMSE'));
   set(gca,'XTick',[1:nhoras],'XTickLabel',{LHORA{1};LHORA{2};LHORA{3};LHORA{4}})
   xlabel('Hora del dia');ylabel('RMSE')

 subplot(2,2,2)
   top=4;
   bar(RMSEDB); 
   axis([0.5 nhoras+0.5 0 top])
   colormap(jrcol);
   title(strcat(VARIABLE,', RMSEDB'));
   set(gca,'XTick',[1:nhoras],'XTickLabel',{LHORA{1};LHORA{2};LHORA{3};LHORA{4}})
   xlabel('Hora del dia');ylabel('RMSEDB')
   
   subplot(2,2,3)
   top=1;
   bar(ECMSS); 
   axis([0.5 nhoras+0.5 0 top])
   colormap(jrcol);
   title(strcat(VARIABLE,', ECMSS'));
   set(gca,'XTick',[1:nhoras],'XTickLabel',{LHORA{1};LHORA{2};LHORA{3};LHORA{4}})
   xlabel('Hora del dia');ylabel('ECMSS')
   
   subplot(2,2,4)
   top=4;
   bar(BIAS); 
   axis([0.5 nhoras+0.5 -top top])
   colormap(jrcol);
   title(strcat(VARIABLE,', BIAS'));
   set(gca,'XTick',[1:nhoras],'XTickLabel',{LHORA{1};LHORA{2};LHORA{3};LHORA{4}})
   xlabel('Hora del dia');ylabel('BIAS')
   
    




annotation('textbox',[0.42 0.001 0.16 0.05],'String',strcat('Estacion=',ESTACION))


pngname=strcat(FIG_PATH,ESTACION,'_',VARIABLE,'_RMSEBIAS.png');
print('-dpng',pngname)


