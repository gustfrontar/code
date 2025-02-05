%**************************************************************************
%       ESTA FUNCION CALCULA LA PROBABILIDAD EN FUNCION DE LA PRECIPITACION
%       PARA UN PRONOSTICO DETERMINISTICO.
%       Script original Febrero 2007
%**************************************************************************

function [p n rango_lluvia] = pre_cal_fun(obs,forecast,umbral,sm_flag)
%**************************************************************************
%sm_flag si la funcion de probabilidad obtenida va a ser suavizada o no
%si es 1 aplico un suavizado, sino no.

a=size(forecast);
ens=a(2);

%Valores de PP que vamos a usar para estimar la funcion de probabilidad.
rango_lluvia=[0 0.01 0.1 0.5 1 2 3 4 5 10 15 20 25 30 50 150];


n_r=length(rango_lluvia);

i_n=find(isnan(forecast)==0);


aux_1=forecast(i_n);
aux_2=obs(i_n);

clear obs forecast

obs=aux_2;
forecast=aux_1;

clear aux_1 aux_2



for i_umb=1:length(umbral)
for i_cat=1:n_r-1
    %Calculo la probabilidad de ocurrencia del fen�meno en funcion de
    %cuanto pronostica la media del ensemble.
    i_1=find(obs > umbral(i_umb) & forecast < rango_lluvia(i_cat+1) & forecast >= rango_lluvia(i_cat));
    i_2=find(forecast < rango_lluvia(i_cat+1) & forecast >= rango_lluvia(i_cat));
    if(length(i_2) > 15)
    p(i_umb,i_cat)=length(i_1)/length(i_2);
    n(i_umb,i_cat)=length(i_2);
    else
        p(i_umb,i_cat)=NaN;
        n(i_umb,i_cat)=NaN;
    end
    %Aca tal vez convendr�a aplicar un suavizado antes de seguir con la
    %calibracion.
    clear i_1 i_2
 
end

p(i_umb,1)=0; %La categoria correspondiente al 0 la anulo.
end

if(sm_flag==1)
    
for iciclos=1:2  %Loop para repetir el suavizado varias veces 
    [filas columnas]=size(p);
    for i=1:filas
       aux=p(i,find(isnan(p(i,:))==0));
       for j=2:length(aux)-1
          p_sm(j)=(aux(j-1)+aux(j)+aux(j+1))/3;   
       end
       %Resuelvo los bordes de forma tal de preserbar la suma de todas las
       %categorias en la version suavizada.
          p_sm(1)=(2*aux(1)+aux(2))/3;
          p_sm(length(aux))=(2*aux(length(aux))+aux(length(aux)-1))/3;
          p(i,find(isnan(p(i,:))==0))=p_sm;
          clear aux p_sm
    end
    %figure
    %plot(rank_hist(1,:))
    %hold on
    %plot(rank_hist_sm(1,:))
    %sum(rank_hist,2)
    %sum(rank_hist_sm,2)
    %p=p_sm;
    

end

end


%**************************************************************************






