clear all
close all
%**************************************************************************
%La idea de este script es generar el mejor Brier de la climatologia para
%el periodo SEPTIEMBRE-DICIEMBRE. En este caso, queremos incluir la
%capacidad de la climatologia para pronosticar los cambios a lo largo del
%periodo y los cambios debido a que la climatologia es diferente para las
%distintas regiones del dominio.
%**************************************************************************
% Juan Ruiz - 2006
%**************************************************************************
%PARAMETROS MODIFICABLES

%Ruta a los archivos:

path_gts='/home/juan/trabajos/TrabajoSLAF/datos_gts/';

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;
numb=length(umbral);

%Extremos del subdominio que vamos a utilizar para la calibracion.

nregs=2;
%Region tropical (menor densidad de datos).
lat_n(1)=0;
lat_s(1)=-25;
lon_e(1)=-40;
lon_w(1)=-80;

%Region centro norte de Argentina 
lat_n(2)=-25;
lat_s(2)=-45;
lon_e(2)=-40;
lon_w(2)=-80;

%Defino algunas variables previas
est_data=[];

undef_gts=999;

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

%Defino la fecha de inicio y fin de los 3 periodos discontinuos.
ini_date{1}='01-Oct-2005';
end_date{1}='31-Dec-2005';
ini_date{2}='01-Oct-2006';
end_date{2}='31-Dec-2006';
ini_date{3}='01-Oct-2007';
end_date{3}='31-Dec-2007';

for id=1:3;  %Tengo que leer 3 periodos discontinuos.
%Genero un número que identifica la fecha de hoy.
date_num=datenum(ini_date{id});
ini_date_num=datenum(ini_date{id});

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date{id});



%COMIENZA EL CICLO EN TIEMPO.

while (date_num <= end_date_num)
    time=date_num-ini_date_num+1; %Contador del tiempo.
    
    date_vec=datevec(date_num);
    fecha=date_vec(1)*10000+date_vec(2)*100+date_vec(3);
    fecha2=num2str(fecha)

%leemos los datos GTS de ese día.  

archivo_datos=strcat(path_gts,'sa_12z.',fecha2);


datos_file=fopen(archivo_datos);
if(datos_file~= -1)              %Pregunto si existe el archivo GTS de ese d�a.
datos=load('-ascii',archivo_datos);

if(~isempty(datos) && fecha==datos(1,1) )            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 

         
%En gts tengo los datos del gts

%Ahora tengo que crear las variables dato 24 y dato 48.

%**************************************************************************
%Uso vecino mas cercano utilizando la funcion find.

  %Encuentro los valores que no son undef
  i_precip=6;
  datos(datos(:,i_precip)==999,i_precip)=0; %Conviernto en 0 los undef (porque no hay 0) (Solo para el GTS).

  i_gts=find(datos(:,i_precip) >=  0 & datos(:,i_precip) ~= 999 & datos(:,i_precip) <= 300);
  clear aux 
  aux=datos(i_gts,:);
  clear datos
  n_aux=length(aux(:,1));
  
  %Ahora genero la variable est_data para este dia.
  
  %El formato de est_data es dia // estacion (ID OMM) // lat // lon
  
  est_data=[est_data ; aux(:,[1:4 6])];
  
  
  
end %Estos 2 end corresponden a la pregunta de si existe el archivo GTS y si la fecha coincide con la de los datos.
end


date_num=date_num+1;
end

end


%Hasta aca leimos los datos de los periodos y los juntamos de forma tal de
%poder calcular el brier de la climatologia para los diferentes umbrales.

%Primero calculamos el Brier de la climatología teniendo en cuenta solo los
%cambios espaciales en la probabilidad de cada umbral (p(u))
%Bc=p(u)(1-p(u)) para cada region y luego hacemos un promedio pesado por
%regiones.


%¿Como tomamos las regiones?? Las tomamos como cajas uniformes (lo ideal
%sería tomar regiones homogeneas, pero esto requiere refinar mucho más los
%cálculos y determinar cuales son las regiones homogeneas!!.
probabilidad=NaN(24,24,numb);
brier_c=NaN(24,24,numb);


boxs=2.5;   %Tamaño de la caja en grados
for i=1:24
    for j=1:24
        lat(i,j)=-60+boxs*i;
        lon(i,j)=-90+boxs*j;
        
        lats=lat(i,j)-boxs/2;
        latn=lat(i,j)+boxs/2;
        lonw=lon(i,j)-boxs/2;
        lone=lon(i,j)+boxs/2;
        
        %Busco las estaciones que estan en la caja i,j
        i_box=( est_data(:,3) > lats & est_data(:,3) <= latn &  est_data(:,4) > lonw & est_data(:,4) <= lone );
        clear aux
        aux=est_data(i_box,5); %Retengo las observaciones que estan dentro de la caja.
        
        %para estos datos tengo que calcular la probabilidad de ocurrencia
        %de cada uno de los umbrales y el brier asociado.
        n(i,j)=sum(i_box);
        if(n(i,j) > 0 ) %Si la caja no está vacia prosigo con el calculo.
            for iumb=1:numb
                i_aux=(aux >= umbral(iumb));   %Cuantas veces llovio por encima del umbral en esta caja?
                probabilidad(i,j,iumb)=sum(i_aux)/n(i,j);  %La probabilidad
                brier_c(i,j,iumb)=probabilidad(i,j,iumb)*(1-probabilidad(i,j,iumb)); %El brier para esta caja.
            end
        end
    end
end
    

  






