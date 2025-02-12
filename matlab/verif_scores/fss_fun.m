%Funcion creada para calcular los fractions skill score (fss)
%Ojo! Requiere que los campos no tengan undefs
function[fss]=fss_fun(obs,forecast,umbral,areas)
%*****************************************************
%obs y forecast son las matrices originales de precipitaci??n observada y
%pronosticada.
%Umbral es un vector fila o columna en el que se definen los umbrales a
%evaluar
%area es un vector que me provee el tamanio 
%Por default el programa va a calcular todos los fss tomando 
%todas las ??reas menores a la indicada en area

%A diferencia del FSS tradicional que se calcula con areas cuadradas, este
%se calcula con areas circulares.
%*******************************************************

%Definimos variables a utilizar
n_umb=length(umbral); %Cantidad de umbrales de pp a evaluar
tam=size(forecast); %Tama??o de las grillas de observaci??n y pron??stico
nxx=tam(2); %Cantidad de puntos en x (zonal)
nyy=tam(1); %Cantidad de puntos en y (meridional)
nzz=tam(3);%Cantidad de tiempos a evaluar (pares prono-observaci??n)
na=length(areas);
%Comienzan las cuentas

tmp_for=NaN(size(obs));
tmp_obs=NaN(size(obs));
prof_for=NaN(size(obs));
prob_obs=NaN(size(obs));

for i_umb=1:n_umb
    
   prob_for=real(forecast > umbral(i_umb));
   prob_obs=real(obs      > umbral(i_umb));
   
  for jj=1:na 
      i_umb
      jj
    tic
    tmp_for=suaviza_fun(prob_for,areas(jj));
    tmp_obs=suaviza_fun(prob_obs,areas(jj));
    toc
    tic
    fss(i_umb,jj)=1 -  nanmean(nanmean(nanmean((tmp_for-tmp_obs).^2)))  / ( nanmean(nanmean(nanmean((tmp_for).^2))) + nanmean(nanmean(nanmean((tmp_obs).^2))) ); 
    toc
  end
  
    
end

