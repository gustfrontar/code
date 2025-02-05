clear all
close all
%**************************************************************************
% Calculamos el valor del pronóstico para pronósticos probabilísticos
% derivados del ensamble, pronósticos probabilísticos deribados de
% pronósticos determinísticos y el valor del pronóstico determinístico que
% no tiene ningun postprocessing.
%**************************************************************************

path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/';

region='sur';

hora=24;

%Con que fuente se calibraron los pronï¿½sticos.
fuente_cal='gts'
%Con que fuente vamos a verificar los pronï¿½sticos.
fuente_ver='gts'

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Parametros para el bootstrap:
n_muestras=1000; %Numero de muestras que vamos a usar para el bootstrap
alfa=5;       %Valor de corte en el que vamos a fijar el lï¿½mite de confianza.

%Nombre de los archivos.

archivo_salida=strcat(path,'slaf2006_value_',region,'_',fuente_cal,fuente_ver,'_',num2str(hora),'.mat')

%Cargo la probabilidad.

load(strcat(path,'slaf2006_prob_',region,'_',fuente_cal,'.mat'))

%Abro este archivo para la verificacion individual de los miembros del
%ensemble.
load(strcat(path,'slaf2006_',region,'_',fuente_ver,'.mat'));

if(hora==24)
ensemble=p24;
end
if(hora==48)
ensemble=p48;
end

%Las distintas calibraciones son:
% 1 - Pronóstico sin calibrar (democratico) en base a los miembros del
% esneble.
% 2 - Pronóstico calibrado según Hamill y Colucci con calibracion dinámica.
% 3 - Pronóstico calibrado según Hamill y Colucci con calibracion estática.
% 4 - Pronóstico probabilístico en base a la media del ensemble (estatico).
% 5 - Pronóstico probabilístico en base al control (estatico).

%Si la fuente de calibracion y verificaciï¿½n es la misma:
if (double(fuente_cal)==double(fuente_ver))
%En este caso uso los datos que estan en el archivo superensemble.
if(hora==24)
prob=prob_nocal_24;
prob2=prob_cal_24;
prob3=prob_cal2_24;
prob4=prob_cal3_24;
prob5=prob_cal4_24;
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
prob3=prob_cal2_48;
prob4=prob_cal3_48;
prob5=prob_cal4_48;
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


else
    
%Si no son los mismos tengo que cargar los datos de la otra fuente. 
%Uso la variable p24 que en la primera columna contiene los datos de
%verificacion y la variable est_data que me dice a que estacion / dia
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
       prob3(n,:)=prob_cal2_24(i,:);
       prob4(n,:)=prob_cal3_24(i,:);
       prob5(n,:)=prob_cal4_24(i,:);
       ensemble_aux(n,:)=ensemble(j,:);
       obs(n,1)=p24(j,1);
   end   
 end
 
 clear ensemble
 ensemble=ensemble_aux;
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
       prob3(n,:)=prob_cal2_48(i,:);
       prob4(n,:)=prob_cal3_48(i,:);
       prob5(n,:)=prob_cal4_48(i,:);
       ensemble_aux(n,:)=ensemble(j,:);
       obs(n,1)=p48(j,1);
   end   
 end
 
 clear ensemble
 ensemble=ensemble_aux;
 clear ensemble_aux
end
    
end

%Primero calculo la media del ensemble

[filas columnas]=size(ensemble);

ensemble(:,columnas+1)=squeeze(nanmean(ensemble(:,2:columnas),2));


% Vamos a calcular el valor asociado a los diferentes pronósticos.

prob_umb=(1:5:100)/100;


%Calculamos el valor para distintos usuarios en base a la funcion value_fun
[realvi1 potvi1 vi1 mprob1 mprobo1 vf1 vc1 vp1]=value_fun(obs,prob,umbral,prob_umb);
[realvi2 potvi2 vi2 mprob2 mprobo2 vf2 vc2 vp2]=value_fun(obs,prob2,umbral,prob_umb);
[realvi3 potvi3 vi3 mprob3 mprobo3 vf3 vc3 vp3]=value_fun(obs,prob3,umbral,prob_umb);
[realvi4 potvi4 vi4 mprob4 mprobo4 vf4 vc4 vp4]=value_fun(obs,prob4,umbral,prob_umb);
[realvi5 potvi5 vi5 mprob5 mprobo5 vf5 vc5 vp5]=value_fun(obs,prob5,umbral,prob_umb);


save(archivo_salida,'realvi1','potvi1','vi1','mprob1','mprobo1','vf1','vc1','vp1')







