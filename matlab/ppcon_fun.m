%Funcion q genera los graficos de contornos para cada pronosticos 
%Se le debe ingresar latitud, longitud, varible y titulo del grafico
%Va junto con el script de cmorphabrir, lo usa plot_fun y tmb contourf2

function [ ] = ppcon_fun(latvar,lonvar,vari,titulo)

valores=[ 1 5 10 20 40 60 80 100]; %Escala del grafico
precision=1; %Valor relativo a la escala
lat_max=max(latvar);
lat_min=min(latvar);
lon_max=max(lonvar);
lon_min=min(lonvar);
axesm(...
         'MapProjection','mercator',...
         'MapLatLimit',[lat_min lat_max],...
         'MapLonLimit',[lon_min lon_max],...
         'labelformat','compass',...
         'grid','on',...
         'fontsize',8);
     
         load coast
                                                                                                                             
plotm(lat,long,'k')
hold on

contourf2(latvar,lonvar,vari,valores,precision) %Llama a una funcion q grafica

title(titulo)
xlabel('Longitud')
ylabel('Latitud')
colorbar('YTick',valores);
