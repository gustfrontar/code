%**************************************************************************
%       ESTA FUNCION CALCULA EL PRONOSTICO PROBABILISTICO PARA UN
%       PRONOSTICO DETERMINISTICO USANDO LA FUNCION DE PROBABILIDAD EN
%       FUNCION DE LA INTESIDAD DE PRECIPITACION CALCULADA POR LA FUNCION
%       PRE_CAL_FUN2.
%**************************************************************************
function [probabilidad] = prob_cal_fun(forecast,p,umbral,rango_lluvia)
%**************************************************************************
% forecast es un vector con todos los pronosticos que queremos pasar a
% probabilidad
% umbral es un vector con los umbrales de precipitacion para los cuales
% vamos a calcular la probabilidad.
% p es una matriz cada fila tiene la probabilidad a distintos valores de
% lluvia, estos valores son los definidos en el vector rango_lluvia. 
%**************************************************************************
a=length(forecast);
n_umb=length(umbral);
n_rango=length(rango_lluvia);

aux=(rango_lluvia(1:n_rango-1)+rango_lluvia(2:n_rango))*0.5;
aux=[0 aux(2:n_rango-1)];
size(aux);

for i=1:a
for i_umb=1:n_umb
    %Para cada pronostico de lluvia, busco en la curva p vs lluvia que
    %valor de probabilidad le corresponde. Para sacar el valor más
    %aproximado uso interpolación bi-lineal.
    i_n=find(isnan(p(i_umb,:))==0);
    if(forecast(i) > 0)
      probabilidad(i,i_umb)=interp1(aux(i_n),p(i_umb,i_n),forecast(i),'linear');
    else
      probabilidad(i,i_umb)=0;
    end
    if(probabilidad(i,i_umb) > 1)
        probabilidad(i,i_umb)=1;
    end
    if(forecast(i) > max(rango_lluvia(i_n)))
        probabilidad(i,i_umb)=max(p(i_umb,i_n));
    end
    %forecast(i)
    %probabilidad(i,i_umb)
end
end

% figure
% p_size=size(p,1);
% color=['r' 'g' 'b' 'y' 'k' 'm' 'c'];
% umb_ral=['0.25' '2.54' '6.35' '12.70' '25.40' '38.10' '50.80'];
% for i=1:p_size
%     hold on
%     plot(aux,p(i,:),color(i),'DisplayName',umb_ral(i));
% end
% legend('0.25','2.54','6.35','12.70','25.40','38.10','50.80',1)
