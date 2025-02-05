clear all
close all
%**************************************************************************
% Prueba para calibrar el pron�stico de la media de manera de obtener un
% pron�stico probabil�stico solo a partir de la media del ensemble.
%**************************************************************************

path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/';

region='sur';


%Con que fuente se calibraron los pron�sticos.
fuente_cal='gts'
%Con que fuente vamos a verificar los pron�sticos.
fuente_ver='gts'

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Parametros para el bootstrap:
n_muestras=100; %Numero de muestras que vamos a usar para el bootstrap
alfa=5;         %Valor de corte en el que vamos a fijar el l�mite de confianza.

%Abro este archivo para la verificacion individual de los miembros del
%ensemble.

load(strcat(path,'slaf2006_',region,'_',fuente_ver,'.mat'));



rango_lluvia=[0 0.001 0.01 0.1 0.5 1 2 3 4 5 10 20 50];

[filas24 columnas24]=size(p24);
[filas48 columnas48]=size(p48);

n_r=length(rango_lluvia);

i_24=find(any(isnan(p24),2)==0);
i_48=find(any(isnan(p48),2)==0);
filas24=length(i_24);
filas48=length(i_48);
media_24=nanmean(p24(i_24,2:columnas24),2);
media_48=nanmean(p48(i_48,2:columnas48),2);
obs_24=p24(i_24,1);
obs_48=p48(i_48,1);

for i_umb=1:length(umbral)
for i_cat=1:n_r-1
    %Calculo la probabilidad de ocurrencia del fen�meno en funcion de
    %cuanto pronostica la media del ensemble.
    i_1=find(obs_24 > umbral(i_umb) & media_24 < rango_lluvia(i_cat+1) & media_24 >= rango_lluvia(i_cat));
    i_2=find(media_24 < rango_lluvia(i_cat+1) & media_24 >= rango_lluvia(i_cat));
    prob_24(i_umb,i_cat)=length(i_1)/length(i_2);
    %Aca tal vez convendr�a aplicar un suavizado antes de seguir con la
    %calibracion.
    clear i_1 i_2
 
end
end

%Voy a generar un pron�stico de precipitacion probabil�stico solo en base a
%la media del ensemble.

for i_umb=1:length(umbral)
for i=1:filas24
    aux=abs(rango_lluvia-media_24(i));
    ind=find(aux==min(aux));
    if(ind==n_r)
        ind=n_r-1;
    end
    probmedia_24(i,i_umb)=prob_24(i_umb,ind); %Interpolo de acuerdo con las curvas calculadas anteriormente mediante vecino mas cercano.
    
end
end



%Vamos a verificar lo que salio...

prob_int=0:0.1:1;
[reliability n_forecast prob_ref clim_prob]= reliability_fun(obs_24,probmedia_24,umbral,prob_int);

[brier]=brier_fun(obs_24,probmedia_24,umbral);

[hit far area ets bias area_ets] = roc_fun(obs_24,probmedia_24,umbral,prob_int);


posicion(1,:)=[0.19 0.77 0.11 0.13]; %Left bottom width heigth



for i_umb=1:length(umbral)
    textito=['umbral ',num2str(umbral(i_umb))];
    figure

   %no_resol_line(1:length(prob_ref))=climp{j}(i);
   plot(prob_ref,reliability(i_umb,:),'r');
   hold on;
   axis([0 1 0 1]);
   %plot(prob_ref,no_resol_line,'g');
   plot(prob_ref,prob_ref,'g');
   % Create axes
   axes('Position',posicion(1,:));
   plot(prob_ref,n_forecast(i_umb,:),'r')
   annotation('textbox','Position',[0.42 0.00 0.16 0.06],'FitHeightToText','on','String',{textito});
end

