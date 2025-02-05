%**************************************************************************
%       ESTA FUNCION CALCULA EL PRONOSTICO PROBABILISTICO A PARTIR DE UN
%       PRONOSTICO DETERMINISTICO Y DE UNA CURVA DE FRECUENCIAS (SEGUN
%       GALLUS Y SEAGAL 2007)
%**************************************************************************
function [probabilidad_out] = prob_cal_fun_1b_noreg(forecast,pup,pub,umbral,powtrans)
%**************************************************************************
% FORECAST_IN ahora es un solo valor, el pronostico para un punto x , y .
%. Tiene otra dimension que representa diferentes plazos de pronostico.
% F_IN es una matriz de 2 filas. La primera contienen frecuencias relativas
% y la segunda el valor de precipitacion al cual corresponden dichas
% frecuencias. Tiene una tercer dimension que marca el umbral, porque las
% frecuencias son frecuencias de precipitacion por encima de un dado
% umbral.
% N_IN es el n???mero total de datos dentro del intervalo y se usa para
% calcular la frecuencia relativa.
% CATEGORIAS son los valores de precipitacion para los que fueron
% calculadas las probabilidades.
% En este caso la funcion esta preparada para calibrar la informacion que
% ingresa, sin discriminar por regiones, tiempos o datos.
% ESTA IMPLEMENTACION USA LA REGRESION LOGISTICA EN LUGAR DE LOS BINS PARA
% REDUCIR EL NUMERO DE PARAMETROS A ESTIMAR Y AUMENTAR LA CONFIABILIDAD DE
% LAS CURVAS. PERO UNA VEZ OBTENIDA LAS REGRESIONES LOGISTICAS PARA CADA
% UMBRAL NO REALIZA UN AJUSTE PARAMETRICO DE SU DEPENDENCIA CON EL UMBRAL.
%**************************************************************************

numb=length(umbral);
probabilidad=NaN(length(forecast),numb); %Predefino la probabilidad como NaN.

%**************************************************************************
%Voy a establecer una serie de criterios para que se pueda llevar a cabo la
%calibracion.
cal=true;
if(sum(isnan(forecast))==length(forecast));cal=false;end
if( sum(isnan(pup))>0 | sum(isnan(pub))>1  );cal=false;end

if(cal)           %Si esta todo en regla largo la calibracion.
  
for i_umb=1:numb
   %Calculamos la pendiente y la ordenada que corresponde al umbral
   %deseado.
   pendiente=pup(i_umb);
   ordenada=pub(i_umb);
   probabilidad(forecast>0,i_umb)=1./(1+exp(-ordenada-pendiente*(forecast(forecast>0).^powtrans))); 
     
end

probabilidad(forecast==0,:)=0;
    
end
    
probabilidad_out(:,:)=probabilidad; 
    

    
   








