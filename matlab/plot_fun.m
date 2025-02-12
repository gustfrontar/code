%Funcion q averigua si hay datos de pp y si los hay hace un grafico de contornos
%Se le debe ingresar latitud, longitud y las variables, q en este caso son 5
%Va junto con el script de cmorphabrir y llama a ppcon_fun

function graf=plot_fun(lati,long,variable_1,variable_2,variable_3,variable_4,variable_5)

figure
vacio72=isnan(variable_1(20,20));
if vacio72~=1
    %subplot (3,2,2)
    ppcon_fun(lati,long,variable_1,'Pp acumulada en 24 hs para el pronostico a 72 hs')
else
end
figure
vacio60=isnan(variable_2(20,20));
if vacio60~=1
    %subplot (3,2,3)
    ppcon_fun(lati,long,variable_2,'Pp acumulada en 24 hs para el pronostico a 60 hs')
else    
end
figure
vacio48=isnan(variable_3(20,20));
if vacio48~=1
    %subplot (3,2,4)
    ppcon_fun(lati,long,variable_3,'Pp acumulada en 24 hs para el pronostico a 48 hs')
else  
end
figure
vacio36=isnan(variable_4(20,20));
if vacio36~=1
    %subplot (3,2,5)
    ppcon_fun(lati,long,variable_4,'Pp acumulada en 24 hs para el pronostico a 36 hs')
else
end
figure
vacio24=isnan(variable_5(20,20));
if vacio24~=1
    %subplot (3,2,6)
    ppcon_fun(lati,long,variable_5,'Pp acumulada en 24 hs para el pronostico a 24 hs')
else
end
