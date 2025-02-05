clear all
close all
%**************************************************************************
%      VerificaciÃ³n de la calibracion del pronostico probabilistico
% Construye reliability diagrams para dos pronósticos probabilisticos
% indicando la probabilidad climatológica y las frecuencias asociadas a
% cada pronostico.
% Construye los ROC diagrams para los distintos pronósticos y los miembros
% del ensemble interesante sobre todo para el superensemble donde tenemos
% una mayor dispersion de hit rate y far en los distintos miembros. Se
% indica la "no skill" line (hit=far) y la linea de far asociada al
% pronóstico puramente estocástico.
% Se calcula tambien el valor de BIAS y ETS para el pronostico realizado
% utilizando diferentes umbrales de probabilidad. Se compara con el ETS y
% el BIAS del mejor pronóstico del ensemble y del peor y el pronóstico de
% la media.
% Proximamente se agregará una estimación del valor económico utilizando
% una actividad de referencia y una tabla de payoff sencilla de forma tal
% de evaluar los cambios en el valor asociados a la calibración.
%**************************************************************************

path='d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/';

region='norte';

hora=48;

%Con que fuente se calibraron los pronósticos.
fuente_cal='gts'
%Con que fuente vamos a verificar los pronósticos.
fuente_ver='gts'

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

archivo_salida=strcat(path,'superensemble_verifprob_',region,'_',fuente_cal,fuente_ver,'_',num2str(hora),'.mat')

%Cargo la probabilidad.

load(strcat(path,'superensemble_prob_',region,'_',fuente_cal,'.mat'))

%Abro este archivo para la verificacion individual de los miembros del
%ensemble.
load(strcat(path,'superensemble_',region,'_',fuente_ver,'.mat'));

if(hora==24)
ensemble=p24;
end
if(hora==48)
ensemble=p48;
end

%Si la fuente de calibracion y verificación es la misma:
if (double(fuente_cal)==double(fuente_ver))
%En este caso uso los datos que estan en el archivo superensemble.
if(hora==24)
prob=prob_nocal_24;
prob2=prob_cal_24;
obs=verif_24(:,2);
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
obs=verif_48(:,2);
%Me quedo con los datos del ensemble que se corresponden con los
%pronosticos de probabilidad, asi comparo sobre los mismos dias y
%estaciones.
 n=0;
 for i=1:length(est_data_48(:,1))
   j=find(est_data(:,1)==est_data_48(i,1) & est_data(:,2)==est_data_48(i,2));
   if(length(j)==1)
       n=n+1;
       ensemble_aux(n,:)=ensemble(j,:);
   end   
 end
 clear ensemble
 ensemble=ensemble_aux;
 clear ensemble_aux
 
end


else
    
%Si no son los mismos tengo que cargar los datos de la otra fuente. 
%Uso la variable p24 que en la primera columna contiene los datos de
%verificacion y la variable est_data que me dice a que estación / dia
%corresponden. 
%est_data(:,1) son las fechas, est_data(:,2) las estaciones.
%Voy tomar la variable est_data_24 o 48 que son las fechas y estaciones 
   
if(hora==24)
 n=0;
 for i=1:length(est_data_24(:,1))
   j=find(est_data(:,1)==est_data_24(i,1) & est_data(:,2)==est_data_24(i,2));
   if(length(j)==1)
       n=n+1;
       prob(n,:)=prob_nocal_24(i,:);
       prob2(n,:)=prob_cal_24(i,:);
       ensemble_aux(n,:)=ensemble(j,:);
       obs(n,1)=p24(j,1);
   end   
 end
 
 clear ensemble
 ensemble=ensemble_aux
 clear ensemble_aux
end
if(hora==48)
 n=0;
 for i=1:length(est_data_48(:,1))
   j=find(est_data(:,1)==est_data_48(i,1) & est_data(:,2)==est_data_48(i,2));
   if(length(j)==1)
       n=n+1;
       prob(n,:)=prob_nocal_48(i,:);
       prob2(n,:)=prob_cal_48(i,:);
       ensemble_aux(n,:)=ensemble(j,:);
       obs(n,1)=p48(j,1);
   end   
 end
 
 clear ensemble
 ensemble=ensemble_aux
 clear ensemble_aux
end
    
end


gifname=strcat(fuente_cal,'_',fuente_ver,'_',region,'_f',num2str(hora),'_');


%Primero calculo la media del ensemble

[filas columnas]=size(ensemble);

ensemble(:,columnas+1)=squeeze(nanmean(ensemble(:,2:columnas),2));


%**************************************************************************
%  CALCULO DE LAS CURVAS DE RELIABILITY PARA EL PRONÃ“STICO PROBABILISTICO
%  CALCULADO PREVIAMENTE.
%**************************************************************************

%Prob int son los intervalos donde voy a acumular las probabilidades
%pronosticadas.

prob_int=0:0.1:1;

[reliability n_forecast prob_ref clim_prob]= reliability_fun(obs,prob,umbral,prob_int);
[reliability_2 n_forecast_2 prob_ref clim_prob_2]= reliability_fun(obs,prob2,umbral,prob_int);

%**************************************************************************
%  CALCULO DE LOS BRIER SCORE
%**************************************************************************

[brier]=brier_fun(obs,prob,umbral);
[brier_2]=brier_fun(obs,prob2,umbral);

%**************************************************************************
%  CALCULO DE LAS CURVAS Y EL "AREA" DEL ROC
%**************************************************************************

prob_umb=0.1:0.1:0.9;

[hit far area ets bias area_ets] = roc_fun(obs,prob,umbral,prob_umb);
[hit_2 far_2 area_2 ets_2 bias_2 area_ets2] = roc_fun(obs,prob2,umbral,prob_umb);

%Calculo hit,far,ets y bias de los miembros del ensemble y su media para
%comparar.

[ets_ens hit_ens far_ens]=ets_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)
[bias_ens]=biasarea_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)

%**************************************************************************
%  GRAFICO LAS CURVAS DEL ROC
%**************************************************************************
roc=1
if(roc==1)
   title_graf=strcat('"AREA" DEL ROC');
   figure
   plot(umbral,area,'r');
   hold on;
   plot(umbral,area_2,'b');

   legend('no cal','cal')
   %axis([0 umbral(length(umbral)) 0 0.3]);
   hold on;
   title(title_graf)
   figura=strcat(path,gifname,'.png');
   print('-dpng',figura)
   for i_umb=1:length(umbral)
   figure
   no_skill=0:0.1:1;
   far_random=ones(length(no_skill),1)*(1-clim_prob(i_umb));
   
   title_graf=strcat('ROC para el umbral ',num2str(umbral(i_umb)));
   plot(far(:,i_umb),hit(:,i_umb),'ro-');
   hold on;
   plot(far_2(:,i_umb),hit_2(:,i_umb),'bo-');
   plot(no_skill,no_skill,'k+')
   hold on
   plot(far_ens(i_umb,1:columnas-1),hit_ens(i_umb,1:columnas-1),'b+');
   hold on
   plot(far_ens(i_umb,columnas),hit_ens(i_umb,columnas),'g+');
   plot(far_random,no_skill,'b+');
   legend('no cal','cal','no skill','members','mean','Random FAR')

   axis([0 1 0 1]);
   hold on;
   title(title_graf)
   figura=strcat(path,gifname,num2str(umbral(i_umb)),'ROC.png');
   print('-dpng',figura)
   end

end
%**************************************************************************
%   GRAFICO LOS RELIABILITY DIAGRAMS.
%**************************************************************************
relig=1
if(relig==1)

for i=1:length(umbral)
    no_resol_line(1:length(prob_ref))=clim_prob(i);
    title_graf=strcat('Reliavility diagram para un umbral de ',num2str(umbral(i)));
    title_graf2=strcat('Frecuencia de pronosticos para un umbral de ',num2str(umbral(i)));
figure
subplot(121)
   
   plot(prob_ref,reliability(i,:),'r');
   hold on;
   plot(prob_ref,reliability_2(i,:),'b');
   axis([0 1 0 1]);
   hold on;
   plot(prob_ref,no_resol_line,'g');
   plot(prob_ref,prob_ref,'g');
   title(title_graf)

subplot(122)
   plot(prob_ref,n_forecast(i,:),'r');
   hold on;
   plot(prob_ref,n_forecast_2(i,:),'b');

   legend('no cal','cal')
   title(title_graf2)
   %axis([0 1 0 max]);
   figura=strcat(path,gifname,num2str(umbral(i)),'_reli.png');
   print('-dpng',figura)
end

end
%**************************************************************************
%   GRAFICO BRIER SKILL SCORE.
%**************************************************************************
bri=1
if(bri==1)
   title_graf=strcat('BRIER SKILL score');
   figure
   plot(umbral,(brier-brier)./brier,'r');
   hold on;
   plot(umbral,(brier_2-brier)./brier,'b');
   legend('no cal','cal')
   axis([0 umbral(length(umbral)) -0.3 0.05]);
   hold on;
   title(title_graf)
   figura=strcat(path,gifname,'BRIER.png');
   print('-dpng',figura)
end  
%**************************************************************************
%   GRAFICO EL ETS PARA EL UMBRAL I_UMB.
%**************************************************************************
etsg=1;
if(etsg==1)
for i_umb=1:length(umbral);
   
       umb=umbral(i_umb);
   %Voy a comparar esto con el maximo y mínimo ETS de los miembros y el de
   %la media.
   
   ets_max=ones(length(prob_umb),1)*max(ets_ens(i_umb,1:columnas-1)); %El maximo ets del ensemble para el umbral i_umb
   ets_min=ones(length(prob_umb),1)*min(ets_ens(i_umb,1:columnas-1)); %El mínimo ets del ensemble para el umbral i_umb
   ets_mean=ones(length(prob_umb),1)*ets_ens(i_umb,columnas);     %Ets de la media
   
   
   title_graf=strcat('ETS umbral ',num2str(umbral(i_umb)));
   figure
   plot(prob_umb,ets(:,i_umb),'r');
   hold on;
   plot(prob_umb,ets_2(:,i_umb),'b');
   legend('no cal','cal','Ens max','Ens min','Ens mean')
   axis([0 1 0 1]);
   hold on;
   plot(prob_umb,ets_max,'bo');
   plot(prob_umb,ets_min,'bo');
   plot(prob_umb,ets_mean,'bo-');
   title(title_graf)
   figura=strcat(path,gifname,'ETS_',num2str(umbral(i_umb)),'.png');
   print('-dpng',figura)
   
   bias_max=ones(length(prob_umb),1)*max(bias_ens(i_umb,1:columnas-1)); %El maximo ets del ensemble para el umbral i_umb
   bias_min=ones(length(prob_umb),1)*min(bias_ens(i_umb,1:columnas-1)); %El mínimo ets del ensemble para el umbral i_umb
   bias_mean=ones(length(prob_umb),1)*bias_ens(i_umb,columnas);     %Ets de la media
   
   title_graf=strcat('BIAS umbral ',num2str(umbral(i_umb)));
   figure
   plot(prob_umb,bias(:,i_umb),'r');
   hold on;
   plot(prob_umb,bias_2(:,i_umb),'b');
   legend('no cal','cal','Ens max','Ens min','Ens mean')
   axis([0 1 0 3]);
   hold on;
   plot(prob_umb,bias_max,'bo');
   plot(prob_umb,bias_min,'bo');
   plot(prob_umb,bias_mean,'bo-');
   title(title_graf)
   figura=strcat(path,gifname,'BIAS_',num2str(umbral(i_umb)),'.png');
   print('-dpng',figura)

   end
end  
%**************************************************************************
%   CALCULO EL VALOR ECONOMICO DEL PRONÓSTICO.
%**************************************************************************
   
%Vamos a calcular el valor economico del pronostico asignando un valor a
%cada situacion prevension / no prevension ocurrencia del fenomeno / no
%ocurrencia del fenómeno. a=no protejo si ocurrencia b= no protejo / no
%ocurre c= protejo y el fenomeno ocurre y d= protejo y el fenomono no
%ocurre. a, b, c, d son los ingresos/costos asociados a cada una de estas
%situaciones.

%Payoff table (valores asociados a cada una e las posibles situaciones.).
a=-5500;
b=2000;
c=-700;
d=-700;

%Si bien existe un valor teórico optimo de tomar la decisión en función de
%los balores a b c d, vamos a calcular el valor del pronóstico utilizando
%diferentes umbrales, ya que de acuerdo a la calibracion el valor de
%probabilidad seleccionado puede bien no corresponder a una probabilidad
%real igual a la óptima. De esta forma obtenemos una matriz de valores del
%pronóstico, para los distintos umbrales de probabilidad y para los
%distintos umbrales de precipitación. La interpretación de tomar diferentes
%umbrales de precipitación, es suponer por ejemplo que la actividad
%desarrollada por la empresa en cuestión solo se ve afectada por las
%condiciones cuando el valor de la variable (en este caso la lluvia) supera
%un determinado valor umbral. 

%Calculamos el valor para los pronósticos probabilísticos en función de
%distintos umbrales de precipitacion y distintos umbrales de probabilidad.
[value opt_prob]=value_fun(obs,prob,umbral,prob_umb,a,b,c,d);
[value_2 opt_prob]=value_fun(obs,prob2,umbral,prob_umb,a,b,c,d);

%Calculamos el maximo valor del pronóstico probabilistico calibrado y sin
%calibrar.

max_value=max(value);
max_value2=max(value_2);

%Calculamos el valor de los pronósticos determinísticos que forman parte
%del ensemble.
[filas columnas]=size(ensemble);
[value_ens opt_prob]=value_fun2(ensemble(:,1),ensemble(:,2:columnas),umbral,a,b,c,d);
[filas columnas]=size(value_ens);
max_value_ens=max(value_ens);
min_value_ens=min(value_ens);
%Calculo el valor del pronóstico nulo

[nul_value]=nul_value_fun(ensemble(:,1),umbral,a,b,c,d);

%Calculo los valores relativos al valor nulo.


value_rel=max_value-nul_value;
value_rel2=max_value2-nul_value;

max_value_ens_rel=max_value_ens-nul_value;
min_value_ens_rel=min_value_ens-nul_value;
value_mean_rel=value_ens(filas,:)-nul_value;

figure
title_graf=strcat('Valor relativo sobre el valor nulo');
plot(umbral,value_mean_rel,'r+-');
hold on
plot(umbral,value_rel,'ro--');
plot(umbral,value_rel2,'bo--');
plot(umbral,max_value_ens_rel,'g--');
plot(umbral,min_value_ens_rel,'g--');
legend('ENS mean','no cal','cal','Max ens','Min ens')


title(title_graf)
figura=strcat(path,gifname,'VALUE.png');
print('-dpng',figura)


%Voy a guardar todas las variables en un archivo.


save(archivo_salida,'reliability','reliability_2','n_forecast','n_forecast_2','clim_prob','brier','brier_2'...
    ,'ets_ens','hit_ens','far_ens','bias_ens','hit','far','area','ets','bias','hit_2','far_2','area_2'...
    ,'ets_2','bias_2','value_rel','value_rel2','max_value_ens_rel','min_value_ens_rel','value_mean_rel'...
    ,'umbral','prob_ref','area_ets','area_ets2')

