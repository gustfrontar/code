clear all
close all
%**************************************************************************
% Verificación de la calibracion del pronostico probabilistico
% Construye reliability diagrams para dos pron�sticos probabilisticos
% indicando la probabilidad climatol�gica y las frecuencias asociadas a
% cada pronostico.
% Construye los ROC diagrams para los distintos pron�sticos y los miembros
% del ensemble interesante sobre todo para el superensemble donde tenemos
% una mayor dispersion de hit rate y far en los distintos miembros. Se
% indica la "no skill" line (hit=far) y la linea de far asociada al
% pron�stico puramente estoc�stico.
% Se calcula tambien el valor de BIAS y ETS para el pronostico realizado
% utilizando diferentes umbrales de probabilidad. Se compara con el ETS y
% el BIAS del mejor pron�stico del ensemble y del peor y el pron�stico de
% la media.
% Esta version en particular lee las salidas de calibracion_slaf2 para
% comparar gts vs cmorph.

%**************************************************************************

for iver=1:4 %Ciclo para verificar las 2 regiones 24 y 48 horas.

    if(iver==1);region='sur';hora=24;end
    if(iver==2);region='sur';hora=48;end
    if(iver==3);region='norte';hora=24;end
    if(iver==4);region='norte';hora=48;end
    
    path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/';

%Con que fuente vamos a verificar los pron�sticos.
fuente_ver='gts'

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Parametros para el bootstrap:
n_muestras=1000; %Numero de muestras que vamos a usar para el bootstrap
alfa=5;       %Valor de corte en el que vamos a fijar el l�mite de confianza.

%Nombre de los archivos.

archivo_salida=strcat(path,'slaf2006_verifprob_',region,'_cmogts_',num2str(hora),'.mat')

%Cargo la probabilidad.

load(strcat(path,'slaf2006_prob_',region,'_cmogts.mat'))

%Abro este archivo para la verificacion individual de los miembros del
%ensemble.
load(strcat(path,'slaf2006_',region,'_',fuente_ver,'.mat'));

if(hora==24)
ensemble=p24;
end
if(hora==48)
ensemble=p48;
end


if(hora==24)
prob=prob_nocal_24;
prob2=prob_cal_24;
prob3=prob_cal2_24;
prob4=prob_cal3_24;
prob5=prob_cal4_24;
obs=verif_24(:,1);
%Me quedo con los datos del ensemble que se corresponden con los
%pronosticos de probabilidad, asi comparo sobre los mismos dias y
%estaciones.
 n=0;
 for i=1:length(est_data_24(:,1))
   j=find(est_data(:,1)==est_data_24(i,1) & est_data(:,2)==est_data_24(i,2));
   if(length(j)==1)
       n=n+1;
       ensemble_aux(n,:)=ensemble(j,:);
   end   
 end
 
 clear ensemble
 ensemble=ensemble_aux;
 clear ensemble_aux
end

if(hora==48)
prob=prob_nocal_48;
prob2=prob_cal_48;
prob3=prob_cal2_48;
prob4=prob_cal3_48;
prob5=prob_cal4_48;
obs=verif_48(:,1);
%Me quedo con los datos del ensemble que se corresponden con los
%pronosticos de probabilidad, asi comparo sobre los mismos dias y
%estaciones.
 n=0;
 for i=1:length(est_data_48(:,1))
   j=find(est_data(:,1)==est_data_48(i,1) & est_data(:,2)==est_data_48(i,2));
   if(length(j)==1)
       n=n+1;
       ensemble_aux(n,:)=ensemble(j,:);
   else
       error=1
       i
       j
   end   
 end
 clear ensemble
 ensemble=ensemble_aux;
 clear ensemble_aux
end


%Primero calculo la media del ensemble

[filas columnas]=size(ensemble);

ensemble(:,columnas+1)=squeeze(nanmean(ensemble(:,2:columnas),2));


%**************************************************************************
%  CALCULO DE LAS CURVAS DE RELIABILITY PARA EL PRONÓSTICO PROBABILISTICO
%  CALCULADO PREVIAMENTE.
%  INCLUYE CURVAS DE SIGNFICANCIA MEDIANTE BOOTSTRAPING
%**************************************************************************

%Prob int son los intervalos donde voy a acumular las probabilidades
%pronosticadas.

prob_int=0:0.1:1;

[reliability n_forecast prob_ref clim_prob]= reliability_fun(obs,prob,umbral,prob_int);
[reliability_2 n_forecast_2 prob_ref clim_prob_2]= reliability_fun(obs,prob2,umbral,prob_int);
[reliability_3 n_forecast_3 prob_ref clim_prob_3]= reliability_fun(obs,prob3,umbral,prob_int);
[reliability_4 n_forecast_4 prob_ref clim_prob_4]= reliability_fun(obs,prob4,umbral,prob_int);
[reliability_5 n_forecast_5 prob_ref clim_prob_5]= reliability_fun(obs,prob5,umbral,prob_int);

%Voy a aplicar el bootstrap test para calcular limites de confianza para
%las curvas del reliability diagram.

[filas2 columnas2]=size(prob);

random_index=bootstrap_fun(filas2,n_muestras);

for i=1:n_muestras
indice=squeeze(random_index(:,i));
[reliability_b(:,:,i) n_forecast_b(:,:,i) prob_ref clim_prob]= reliability_fun(obs(indice),prob2(indice,:),umbral,prob_int);  
end

%Primero ordenamos la variable de mayor a menor
reliability_b=sort(reliability_b,3,'ascend');

%Calculo cuantos valores de la muestra deben quedar por encima o por debajo
%para que se verifique que se correspondan con el alfa seleccionado.
i_corte=round(n_muestras*alfa/(2*100)); 

%Interpolo el valor del ets correspondiente al valor de corte superior e
%inferior.
reliability_inf=(reliability_b(:,:,i_corte+1)+reliability_b(:,:,i_corte-1))/2;
reliability_sup=(reliability_b(:,:,n_muestras-i_corte-1)+reliability_b(:,:,n_muestras-i_corte+1))/2;

%**************************************************************************
%  CALCULO DE LOS BRIER SCORE
%**************************************************************************
brierkk=1


[brier]=brier_fun(obs,prob,umbral);
[brier_2]=brier_fun(obs,prob2,umbral);
[brier_3]=brier_fun(obs,prob3,umbral);
[brier_4]=brier_fun(obs,prob4,umbral);
[brier_5]=brier_fun(obs,prob5,umbral);
[brier_clim brier_ens]=brier_fun2(obs,ensemble(:,2:columnas+1),umbral);

for i=1:n_muestras
indice=squeeze(random_index(:,i));
[brier_b(:,i)]= brier_fun(obs(indice),prob2(indice,:),umbral);  
end

%Primero ordenamos la variable de mayor a menor
brier_b=sort(brier_b,2,'ascend');

%Calculo cuantos valores de la muestra deben quedar por encima o por debajo
%para que se verifique que se correspondan con el alfa seleccionado.
i_corte=round(n_muestras*alfa/(2*100)); 

%Interpolo el valor del brier correspondiente al valor de corte superior e
%inferior.
brier_inf=(brier_b(:,i_corte+1)+brier_b(:,i_corte-1))/2;
brier_sup=(brier_b(:,n_muestras-i_corte-1)+brier_b(:,n_muestras-i_corte+1))/2;

%**************************************************************************
%  CALCULO DE LAS CURVAS Y EL "AREA" DEL ROC
%**************************************************************************

prob_umb=0.1:0.1:0.9;

[hit far area ets bias area_ets] = roc_fun(obs,prob,umbral,prob_umb);
[hit_2 far_2 area_2 ets_2 bias_2 area_ets2] = roc_fun(obs,prob2,umbral,prob_umb);
[hit_3 far_3 area_3 ets_3 bias_3 area_ets3] = roc_fun(obs,prob3,umbral,prob_umb);
[hit_4 far_4 area_4 ets_4 bias_4 area_ets4] = roc_fun(obs,prob4,umbral,prob_umb);
[hit_5 far_5 area_5 ets_5 bias_5 area_ets5] = roc_fun(obs,prob5,umbral,prob_umb);

%Calculo hit,far,ets y bias de los miembros del ensemble y su media para
%comparar.

[ets_ens hit_ens far_ens]=ets_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)
[bias_ens]=biasarea_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)

%Calculo las curvas del ROC a partir de los miembros del ensemble, esto se
%hace usando diferentes umbrales de pron�stico para un mismo umbral de
%precipitacion (i.e. manipulando el BIAS del pron�stico deterministico) de
%esta manera obtenemos una curva similar a la del pron�stico
%probabil�stico. Para mayores detalles ver descripci�n en la funci�n
%roc_fun2.

[hit_det far_det area_det ets_det bias_det area_ets_det] = roc_fun2(obs,ensemble(:,2:columnas+1),umbral);

%Vamos a tomar el miembro con el mayor area_ets, con la menor y la media
%del ensemble y lo mismo para el digrama ROC.

%Busco para cada umbral el miembro con la m�xima area del ROC y del ETS
%BIAS y el que tiene la minima area del ROC y del ETS BIAS.
area_ets_max=nanmax(area_ets_det(:,1:columnas-1),[],2);
area_ets_min=nanmin(area_ets_det(:,1:columnas-1),[],2);
area_max=max(area_det(:,1:columnas-1),[],2);
area_min=min(area_det(:,1:columnas-1),[],2);
for i_umb=1:length(umbral)

i_ens_maxets(i_umb)=find(area_ets_det(i_umb,:)==area_ets_max(i_umb));
i_ens_minets(i_umb)=find(area_ets_det(i_umb,:)==area_ets_min(i_umb));
i_ens_max(i_umb)=find(area_det(i_umb,:)==area_max(i_umb));
i_ens_min(i_umb)=find(area_det(i_umb,:)==area_min(i_umb));
ets_max(:,i_umb)=ets_det(:,i_umb,i_ens_maxets(i_umb));
ets_min(:,i_umb)=ets_det(:,i_umb,i_ens_minets(i_umb));
bias_max(:,i_umb)=bias_det(:,i_umb,i_ens_maxets(i_umb));
bias_min(:,i_umb)=bias_det(:,i_umb,i_ens_minets(i_umb));
hit_max(:,i_umb)=hit_det(:,i_umb,i_ens_max(i_umb));
hit_min(:,i_umb)=hit_det(:,i_umb,i_ens_min(i_umb));
far_max(:,i_umb)=far_det(:,i_umb,i_ens_max(i_umb));
far_min(:,i_umb)=far_det(:,i_umb,i_ens_min(i_umb));
end

ets_mean(:,:)=ets_det(:,:,columnas);
bias_mean(:,:)=bias_det(:,:,columnas);
hit_mean(:,:)=hit_det(:,:,columnas);
far_mean(:,:)=far_det(:,:,columnas);
far_mean(:,:)=far_det(:,:,columnas);

clear i_ens_maxets i_ens_minets i_ens_max i_ens_min area_ets_max area_ets_min area_min area_max


%**************************************************************************
%   CALCULO LA DISPERSION DEL ENSAMBLE Y EL MSE DE LA MEDIA.
%**************************************************************************



for i_aux=2:columnas
   diff_aux(:,i_aux)=ensemble(:,i_aux)-ensemble(:,columnas+1); 
end
spread=mean(nanmean((diff_aux).^2,2))^0.5;
mean_mse= (mean((ensemble(:,columnas+1)-ensemble(:,1)).^2))^0.5;

%**************************************************************************

%Voy a guardar todas las variables en un archivo.


save(archivo_salida,'reliability','reliability_2','reliability_3','reliability_4','reliability_5','n_forecast_2','n_forecast','n_forecast_4','n_forecast_5'...
    ,'n_forecast_3','clim_prob','brier','brier_2','brier_5'...
    ,'brier_3','brier_4','ets_ens','hit_ens','far_ens','bias_ens','hit','far','area','ets','bias'...
    ,'hit_2','far_2','area_2','ets_2','bias_2','hit_3','far_3','area_3','ets_3','bias_3','hit_4','far_4','area_4','ets_4','bias_4'...
    ,'hit_5','far_5','area_5','ets_5','bias_5','area_det','area_ets_det'...
    ,'umbral','prob_ref','area_ets','area_ets2','area_ets3','area_ets4','area_ets5','reliability_inf','reliability_sup','brier_inf','brier_sup'...
    ,'brier_clim','brier_ens','ets_max','ets_min','bias_max','bias_min','hit_max','hit_min','far_max','far_min'...
    ,'ets_mean','bias_mean','hit_mean','far_mean','spread','mean_mse')

    clear all

end %End sobre iver





