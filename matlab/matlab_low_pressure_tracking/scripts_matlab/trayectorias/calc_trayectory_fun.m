
function [trayectorias]=calc_trayectory_fun(input,config)
%Juan Ruiz 2010

%Esta funcion recibe datos en una estructura (input) y una serie de
%parametros en una estructura (config). Y a partir de eso calcula las
%trayectorias de sistemas.

%Los elementos de input deben ser:
% Data: un array tridimensional con anomalias estandarizadas y filtradas.
% Data_unfilt: un array cuatridimensional (lat,lon,tiempo,variables) donde 
% esta el valor de las variables que van a ser promediadas y donde se
% buscaran los extremos para cada sistema. Aca irian los campos no
% filtrados de las variables.
% lat: Un array 2D con las latitudes.
% lon: Un array 2D con las longitudes.
% times: Un vector con las fechas (en formato numero de matlab).

%==========================================================================
%Seteamos la configuracion por defecto
%Esto sirve ademas como una referencia de los cambos que habria que se
%pueden modificar en la configuracion.
%==========================================================================

if( ~isfield(config,'id_type'))
    config.id_type=2;         %Tipo de id de sistemas, 1=geop y laplaciano, 2=minimo revestido
end
if( ~isfield(config,'umb_anom'))
    config.umb_anom=-50;       %Umbral en anomalia para definir el objeto.
end
if( ~isfield(config,'umb_min'))  %Umbral para definir un minimo en el algoritmo del minimo revestido.
    config.umb_min=-50;
end
if( ~isfield(config,'maxsissize')) %Extension maxima en puntos de reticula en la que un sistema se puede extender en X o Y en el metodo del minimo revestido.
    config.maxsissize=50;
end
if( ~isfield(config,'umb_lap'))
    config.umb_lap=0.1;       %Umbral en laplaciano para definir el objeto.
end
if( ~isfield(config,'umbral_distancia'))
    config.umbral_distancia=800*1e3; %A partir de que distancia vamos a decir que un sistema es el mismo que 6 horas antes.
end
if( ~isfield(config,'umbral_area'))
    config.umbral_area=3.14*(400000^2);  %A partir de que umbral de area vamos a retener el sistema.
end
if( ~isfield(config,'intervalo'))
    config.intervalo=6;                %Intervalo en horas cada cuanto estan los campos.
end
if( ~isfield(config,'global'))          %Si el dominio es global o no.
    config.global=true;
end 
if( ~isfield(config,'borderdelete'))    %Si el dominio es regional, si borramos los sistemas que estan en el borde o no.
    config.borderdelete=false;
end
if( ~isfield(config,'umb_border'))      %Cuantos puntos voy a "permitir" que esten en contacto con el borde.
    config.umb_border=1;
end
if( ~isfield(config,'umbral_percent'))      %Umbral para la coincidencia entre sistemas para determinar "splittings" ficticios.
    config.umbral_percent=0.6;
end
if( ~isfield(config,'splitcheck'))      %Detecto y remuevo los splits ficticios?
    config.splitcheck=true;
end
if( ~isfield(config,'config.umb_minbarrier'))      %Barrera de geopotencial minima que tiene que existir entre dos minimos para que sean consdierados diferentes.
    config.umb_minbarrier=10;
end


%Parametros que faltan: un parametro que diga si el dominio es global (en
%cuyo caso se aplicaran condiciones de borde especiales) o si es regional
%en cuyo caso podemos eliminar los sistemas que esten en contacto con el
%borde o retenerlos...)
%==========================================================================
%Indentificamos los sistemas presentes en cada tiempo.
%==========================================================================


[trayectorias.mask]=system_id_fun(input,config);


%Aca habria que poner tambien el otro metodo y meter todo en un if que nos
%permita elegir un metodo o el otro.

%==========================================================================
%Calculamos las trayectorias intentando asociar los sistemas presentes en
%el tiempo T con aquellos presentes en el T+1
%==========================================================================


[trayectorias]=trayectory_fun(trayectorias,input,config);


%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================




