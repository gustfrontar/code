clear all
%close all
%**************************************************************************
%
%**************************************************************************
% Juan Ruiz - 2006
%**************************************************************************
%PARAMETROS MODIFICABLES

%Ruta a los archivos:

%Archivo donde guardamos los resultados.
archivo1='d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/AVNxx.mat'

load(archivo1);

%umbrales verificacion
%variable vectorial con los diversos umbrales que vamos a usar en la
%verificación. Por ejemplo para el calculo del ETS. Los umbrales se definen
%como en Hamill y Colucci.

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Extremos del subdominio que vamos a utilizar para la calibracion.

%Region tropical (menor densidad de datos).
%lat_n=0;
%lat_s=-20;
%lon_e=-50;
%lon_w=-80;


%Region centro norte de Argentina 
lat_n=-25;
lat_s=-45;
lon_e=-50;
lon_w=-80;


%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras� letras en ingles)

aux=precip24(find(precip24(:,3)<=lat_s & precip24(:,3) <=lat_n),:);
aux2=aux(find(aux(:,4)<=lon_e & aux(:,4) <=lon_w),:);
clear precip 24
precip24=aux2;
clear aux aux2

aux=precip48(find(precip48(:,3)>=lat_s & precip48(:,3) <=lat_n),:);
aux2=aux(find(aux(:,4)<=lon_e & aux(:,4) >=lon_w),:);
clear precip 24
precip24=aux2;
clear aux aux2

%**************************************************************************
%COMIENZAN LOS CALCULOS DERIBADOS DE LOS DATOS OBTENIDOS.
%**************************************************************************


%Calculo el RMSE para los datos seleccionados.
%**************************************************************************


%Calculo del BIAS para los datos seleccionados. (BIAS en valor)
%**************************************************************************

bias_24=nanmean(precip24(:,5))/nanmean(precip24(:,6));
bias_48=nanmean(precip48(:,5))/nanmean(precip48(:,6));

%**************************************************************************
%Calculo del BIAS en area para distintos umbrales de precipitacion.
%**************************************************************************


for iumb=1:n_umb
    for iens=1:ens+1

%Para 24 horas
clear iarea_sal iarea_cmo
iarea_sal=find(dato_24(:,1) >= umbral(iumb));
iarea_cmo=find(dato_24(:,iens+1) >= umbral(iumb));
bias_area_24(iumb,iens)=length(iarea_cmo)/length(iarea_sal);

%Para 48 horas
clear iarea_sal iarea_cmo
iarea_sal=find(dato_48(:,1) >= umbral(iumb));
iarea_cmo=find(dato_48(:,iens+1) >= umbral(iumb));
bias_area_48(iumb,iens)=length(iarea_cmo)/length(iarea_sal);

    end
end


%Histograma de frecuencias de lluvias.
%**************************************************************************
%Este histograma muestra de alguna manera el BIAS en el area asociado a
%distintos rangos de datos. El tratamiento es por rangos a diferencia de
%BIAS que es por umbral. El tratamiento por rangos es más adecuado y más
%exigente con el pronóstico de precipitación.
%En este caso calculamos un histograma para el CMORPH, y uno para cada uno
%de los miembros del ensemble y su media.

resol=5;
precip_max=50;
precip_top=resol;
precip_bot=0;
n=1;
while (precip_top <= precip_max)
    precip_top=precip_bot+resol;
    for iens=1:ens+1
    clear i_sall i_cmo %para 24 horas de pronóstico
    i_wrf=find(dato_24(:,iens+1) <= precip_top & dato_24(:,iens+1) >= precip_bot);
    i_cmo=find(dato_24(:,1) <= precip_top & dato_24(:,1) >= precip_bot);
    histo_24(n,1)=length(i_cmo);
    histo_24(n,iens+1)=length(i_wrf);
    clear i_sall i_cmo %para 48 horas de pronostico
    i_wrf=find(dato_48(:,iens+1) <= precip_top & dato_48(:,iens+1) >= precip_bot);
    i_cmo=find(dato_48(:,1) <= precip_top & dato_48(:,1) >= precip_bot);
    histo_48(n,iens+1)=length(i_wrf);
    histo_48(n,1)=length(i_cmo);
    end
    n=n+1;axis([1 50 0 1500])
    precip_bot=precip_bot+resol;
end

pp=resol/2:resol:precip_max+resol/2;
figure
plot(pp,histo_24(:,1),'b',pp,histo_24(:,2),'g')
axis([1 50 0 100]);
title('Histograma 24 Azul: Datos, Verde: Pronóstico control')
figure
plot(pp,histo_48(:,1),'b',pp,histo_48(:,2),'g')
axis([1 50 0 100]);
title('Histograma 48 Azul: Datos, Verde: Pronóstico control')
end

%**************************************************************************
%INICIO EL CALCULO DE DIVERSOS PARAMETROS PARA LOS UMBRALES SELECCIONADOS.
%Vamos a calcular aciertos, falsas alarmas, ETS y sorpresas.
n_umb=length(umbral); %Vemos cuantos umbrales distintos tenemos.

[ets_24,hit_rate_24,far_24]=ets_fun(dato_24(:,1),dato_24(:,2:ens+2),umbral);

[ets_48,hit_rate_48,far_48]=ets_fun(dato_48(:,1),dato_48(:,2:ens+2),umbral);


%Calculos relacionados con la probabilidad...
%**************************************************************************

   a=size(dato_24(i_nonnan24,:));
   
   prob24=zeros(a(1),n_umb);
   
   a=size(dato_48(i_nonnan48,:));
   
   prob48=zeros(a(1),n_umb);
   
   %Calculo las probabilidades del ensemble en base a los pronósticos de
   %cada uno de los miembros
   %***********************************************************************
   
   %aux_24=dato_24(i_nonnan24,2:ens+1);
   %aux_48=dato_48(i_nonnan48,2:ens+1);
   %aux3=length(aux_24(:,1));
   
   for i_umb=1:n_umb
    for iens=1:ens
    %Forma mas eficiente de calcular la probabilidad vectorizando loops.   
    prob24(find(dato_24(i_nonnan24,iens+1) >= umbral(i_umb)),i_umb)=prob24(find(dato_24(i_nonnan24,iens+1) >= umbral(i_umb)),i_umb)+1;
    prob48(find(dato_48(i_nonnan48,iens+1) >= umbral(i_umb)),i_umb)=prob48(find(dato_48(i_nonnan48,iens+1) >= umbral(i_umb)),i_umb)+1;      
    % for j=1:aux3
    %     if (aux_24(j,iens) >= umbral(i_umb))
    %    prob24(j,i_umb)=prob24(j,i_umb)+1;
    %     end    
    % end
    
    
    end
   end
   
   %***********************************************************************
   %Calculo las curvas de verificacion del pronostico probabilistico.
   %Calculo la probabilidad observada asociada a cada una de las categorias
   %de la probabilidad pronosticada.
   %***********************************************************************
   %verificacion24=squeeze(dato_24(i_nonnan24,1));
   
   for i_umb=1:n_umb
   
       for iens=1:ens+1
           %Calculo el total de veces que la probabilidad es iens y que
           %ademas llovio por encima del umbral
           i_1=find(dato_24(i_nonnan24,1) >= umbral(i_umb) & prob24(:,i_umb) ==iens-1);
           %calculo el total de veces en que la probabilidad es iens
           i_2=find(prob24(:,i_umb)==iens-1); 
           %length(i_1)
           %length(i_2)
           if(length(i_2)>0)
           reli_24(i_umb,iens,1)=length(i_1)/length(i_2);
           reli_24(i_umb,iens,2)=length(i_2);
           else
           reli_24(i_umb,iens,1)=NaN;
           end
           clear i_1 i_2
           
           %Calculo el total de veces que la probabilidad es iens y que
           %ademas llovio por encima del umbral
           i_1=find(dato_48(i_nonnan48,1) >= umbral(i_umb) & prob48(:,i_umb) ==iens-1);
           %calculo el total de veces en que la probabilidad es iens
           i_2=find(prob48(:,i_umb)==iens-1); 
           %length(i_1)
           %length(i_2)
           if(length(i_2)>0)
           reli_48(i_umb,iens,1)=length(i_1)/length(i_2);
           reli_48(i_umb,iens,2)=length(i_2);
           else
           reli_48(i_umb,iens,1)=NaN;
           end
           clear i_1 i_2          
                    
           
       end
     
   end
   
   %Graficado de los reliability diagrams
   prob=0:1/ens:1;
   
   %Para 24 horas.
   figure;
   contourf(prob,umbral,reli_24(:,:,1),prob);
   colormap(jet(10));
   caxis([0 1]);
   run colorbar;
   title('Reliavility 24')
   xlabel('Probabilidad pronosticada')
   ylabel('Umbral')
   %Para a48 horas.
   figure;
   contourf(prob,umbral,reli_48(:,:,1),prob);
   colormap(jet(10));
   caxis([0 1]);
   run colorbar;
   title('Reliavility 48')
   xlabel('Probabilidad pronosticada')
   ylabel('Umbral')
   
   %Calculo el BRIER score para verificar el pronóstico de probabilidad.
   %***********************************************************************
   
   for i_umb=1:n_umb
       aux=zeros(length(i_nonnan24),1);
       aux(find(dato_24(i_nonnan24,1) >= umbral(i_umb)))=1; %Aux representa la observacion que es 0 o 1.
       brier_24(i_umb)=nanmean((prob24(:,i_umb)/ens-aux).^2);
       clear aux
       
       aux=zeros(length(i_nonnan48),1);
       aux(find(dato_48(i_nonnan48,1) >= umbral(i_umb)))=1; %Aux representa la observacion que es 0 o 1.
       brier_48(i_umb)=nanmean((prob48(:,i_umb)/ens-aux).^2);
       clear aux
       
       %En esta parte faltaría calcular las componentes del BRIER y
       %eventualmente el BRIER SKILL SCORE.
   end
   
   %Fin del calculo del BRIER SCORE.
   %***********************************************************************
   
   
   %Calculo de los diagramas ROC. 
   %***********************************************************************
   %Vamos a calcular para distintos umbrales, como varía el coeficiente de
   %acierto y el de falsa alarma en funcion del umbral de probabilidad
   %utilizado para la toma de decisión en la detección del fenomeno.
   
   for i_umb=1:n_umb               %Loop sobre los umbrales
       
       for iprob=1:ens             %Loop sobre las probabilidades.
           %Calculamos el hit_rate y la falsa alarma para cada umbral y
           %para cada valor de probabilidad seleccionado como umbral de
           %corte.
           %El acierto ahora se define cuando la probabilidad pronosticada
           %es mayor o igual que el umbral (iprob) y además el fenomeno
           %tuvo lugar.
           
    i_hits=find(dato_24(i_nonnan24,1) >= umbral(i_umb) & prob24(:,i_umb) >= iprob); 
    i_total=find(dato_24(i_nonnan24,1) >= umbral(i_umb));
    if(length(i_total) > 0)
    roc_hit24(iprob,i_umb)=length(i_hits)/length(i_total);
    else
    roc_hit24(iprob,i_umb)=NaN;
    end
    i_false=find( dato_24(i_nonnan24,1)  < umbral(i_umb) & prob24(:,i_umb) >= iprob); 
    if(length(i_hits)+length(i_false) > 0)
    roc_false24(iprob,i_umb)=length(i_false)/(length(i_hits)+length(i_false));
    else
    roc_false24(iprob,i_umb)=NaN;
    end
    clear i_hits i_total i_false
    

    
    i_hits=find(dato_48(i_nonnan48,1) >= umbral(i_umb) & prob48(:,i_umb) >= iprob); 
    i_total=find(dato_48(i_nonnan48,1) >= umbral(i_umb));
    if(length(i_total) > 0)
    roc_hit48(iprob,i_umb)=length(i_hits)/length(i_total);
    else
    roc_hit48(iprob,i_umb)=NaN;
    end
    i_false=find( dato_48(i_nonnan48,1)  < umbral(i_umb) & prob48(:,i_umb) >= iprob); 
    if(length(i_hits)+length(i_false) > 0)
    roc_false48(iprob,i_umb)=length(i_false)/(length(i_hits)+length(i_false));
    else
    roc_false48(iprob,i_umb)=NaN;
    end
    clear i_hits i_total i_false
           
                      
       end
   end
   
   %Ploteo los resultados en un diagrama ejex falsa alarma eje y acierto.
    figure
    plot(roc_false24(:,:),roc_hit24(:,:))
    legend('1','5','10','15','20','25','30','35','40','45','50')
    axis([0 1 0 1]);
    xlabel('Falsa alarma')
    ylabel('Acierto')
    title('ROC 24')
    figure
    plot(roc_false24(:,:),roc_hit48(:,:))
    legend('1','5','10','15','20','25','30','35','40','45','50')
    xlabel('Falsa alarma')
    axis([0 1 0 1]);
    ylabel('Acierto')
    title('ROC 48')
    
    
    %Fin del calculo de los diagrama ROC.
    
  
   
     
     



save(archivo3,'ets_24','ets_48','reli_24','reli_48','far_24','far_48','hit_rate_24','hit_rate_48','histo_24','histo_48','bias_24','bias_48','bias_area_24','bias_area_48','rmse_24','rmse_48','dato_24','dato_48','roc_hit24','roc_hit48','roc_false24','roc_false48')