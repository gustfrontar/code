clear all
close all
%*************************************************************************
%Vamos a usar los datos SALLJEX interpolados y los datos CMORPH
%interpolados para realizar un estudio sobre la posible calibracion de los
%datos CMORPH para el perido SALLJEX.
%La idea sera comparar CMORPH vs SALLJEX para ver si existe una relacion
%entre ambas.
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta del directorio donde estan los archivos
archivo='D:/SALLJEXprecip/precip_cmorph_1x1.mat'
archivo2='D:/SALLJEXprecip/precip_salljex_1x1.mat'

load(archivo) 
load(archivo2)

%Extremos del dominio total en donde se promediaron los valores de pp.
lat_min=-60;
lat_max=0;
lon_min=-80; 
lon_max=-40;

%Resoluci�n de los datos que estamos considerando para la calibracion en
%grados.

box_size=1;

%Con que orden vamos a fitear los datos obtenidos.

fit_order=1;

%Umbral de datos (numero m�nimo de datos en las cajas salljex que vamos a
%exigir para que el punto intervenga en la calibracion).

umbral_n=5;

%Extremos del subdominio que vamos a utilizar para la calibracion.

lat_n=-20;
lat_s=-40;
lon_e=-55;
lon_w=-67;

undef=-9999

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='15-Nov-2002';
end_date='15-Feb-2003';


%Genero un número que identifica la fecha de hoy.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);
old_file=-1;

n_datos=1; %Cuenta el n�mero de pares que vamos a obtener para la calibracion.
[ny nx]=size(squeeze(precip_sall(1,:,:,1)))
%COMIENZA EL CICLO EN TIEMPO.
while (date_num <= end_date_num)
    time=date_num-ini_date_num+1 %Contador del tiempo.
    n_ini=n_datos; %Me permite saber cual es el n inicial de este d�a.
  for ilon=1:nx
      for ilat=1:ny
         lat_field=lat_min+box_size/2+(ilat-1)*box_size; %Asumimos como que el valor del pixel corresponde al centro de las cajas definidas.
         lon_field=lon_min+box_size/2+(ilon-1)*box_size;

          if(precip_sall(time,ilat,ilon,1) ~= undef & precip_cmo(time,ilat,ilon,1)  ~= undef)
             if(precip_sall(time,ilat,ilon,1)==0 & precip_cmo(time,ilat,ilon,1)==0)
             else
                 if(lat_field >= lat_s & lat_field <= lat_n & lon_field >= lon_w & lon_field <= lon_e) %Solo considero los datos en una subregion.
                     if(precip_sall(time,ilat,ilon,2) >= umbral_n)
                         if(precip_cmo(time,ilat,ilon,1)>= 0 & precip_cmo(time,ilat,ilon,1)<=300)
                     dato(n_datos,1)=precip_sall(time,ilat,ilon,1); %Genero una serie con el total.
                     dato(n_datos,2)=precip_cmo(time,ilat,ilon,1);
                     n_datos=n_datos+1;
                         end
                     end
                 end
             end
          end
      end
  end
  %Una vez que termino cada d�a, computo un coeficiente de correlacion y un
  %numero de datos asociado a este d�as. Luego puedo sacar un r total, pero
  %la idea es ver tambi�n como varia la correlaci�n espacial entre los
  %campos para diferentes dias. Y ver que d�as la correlaci�n es mejor y
  %tal vez intentar filtrar los dias donde la correlaci�n es muy mala para
  %obtener una curva de calibraci�n.
  clear aux aux2
  if (n_datos > 1 & n_datos-n_ini > 1)
  aux(:,1)=dato(n_ini:n_datos-1,1);
  aux(:,2)=dato(n_ini:n_datos-1,2);
  aux2(:,:)=corr(aux);
  corr_coef(time,1)=aux2(1,2);
  corr_coef(time,2)=n_datos-n_ini;
  end
  
  
date_num=date_num+1;
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!

scatter(dato(:,1),dato(:,2),1)
[P,S]=polyfit(dato(:,1),dato(:,2),fit_order); %Uso la funcion POLYFIT para ajustar un poliniomio de grado 1 a los datos usando minimos cuadrados.
x_lluvia=0:0.5:120; %Genero un x donde voy a evaluar el polinomio que obtuve.
y_lluvia=polyval(P,x_lluvia); %Obtengo los y para los x seleccionados del polinomio P que es la recta de cuadrados minimos en este caso.

hold on; %Grafico sobre el mismo gr�fico.

scatter(x_lluvia,y_lluvia,2); %Grafico la recta de regresi�n (calibracion lineal).

