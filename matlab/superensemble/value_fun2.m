%**************************************************************************
%    FUNCION DEL CALCULO DEL VALOR DE UN PRONÓSTICO (PARA PRONOSTICOS DETERMINISTICOS). 
%**************************************************************************
function [value opt_prob]=value_fun(obs,ensemble,umbral,a,b,c,d)

%**************************************************************************
%           obs= contiene las observaciones (mm)
%           ensemble = contiene en cada culumna un pronostico
%           deterministico (pueden ser los miembros de un esnemble o
%           simplemente pronóstico que se quieran comparar).
%           umb= vector con los umbrales para los cuales vamos a calcular
%           el valor
%           Nan Ready (R).
%           La respuesta valor tiene tantas filas como pronósticos y tantas
%           columnas como umbrales. Cada fila representa el valor del
%           pronóstico i para los valores de a, b, c y d ingresados.
%**************************************************************************

%***********************************************************************
%Vamos a calcular para distintos umbrales, como varÃ­a el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decisiÃ³n en la detecciÃ³n del fenomeno.

[filas columnas]=size(ensemble);
   
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       for icol=1:columnas     %Loop sobre los pronósticos.
%Para cada pronostico calculamos los aciertos y las falsas
%alarmas.
    i=find(isnan(obs)==0 & isnan(ensemble(:,icol))==0 );
    prono=ensemble(i,icol);
    aux_obs=obs(i);

           
    i_hits=find(aux_obs >= umbral(i_umb) & prono >= umbral(i_umb));
    i_hits2=find(aux_obs < umbral(i_umb) & prono < umbral(i_umb));
    i_false=find(aux_obs < umbral(i_umb) & prono >= umbral(i_umb));
    i_miss=find(aux_obs >= umbral(i_umb)  & prono < umbral(i_umb));
    
    hit(icol,i_umb)=length(i_hits);
    hit2(icol,i_umb)=length(i_hits2);   
    false(icol,i_umb)=length(i_false);
    misses(icol,i_umb)=length(i_miss);
    total(icol,i_umb)=length(aux_obs);

    clear i_hits i_false i_hits2 i_miss prono aux_obs i
                       
       end
   end

   %Valor del pronostico en u$s por dia.
   value=(a*misses+b*hit2+c*hit+d*false)./total;
   opt_prob=(b-d)/((b-d)+(c-a));