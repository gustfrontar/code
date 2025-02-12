clear all
close all
%*************************************************************************
%Este programa lee los datos del CPC y completa para cada dato con los
%pronósticos de cada uno de los modelos.
%La idea es hacer un do sobre las fechas, para cada fecha abro el archivo
%del CPC correspondiente, verifico que esté bien la fecha de este archivo.
%Si es asi comienzo a leer las estaciones y busco las mismas estaciones en
%el archivo del modelo correspondiente y asi voy construyendo una matriz
%con observaciones y pronosticos que verifican al mismo tiempo de la
%observacion. No tomamos regiones por el momento. Si una estacion no esta
%en el pronostico se le asigna un NaN. Luego se trabajará con aquellas
%estaciones que esten disponibles para todos los modelos.
%Por el momento usamos solamente pronosticos de las 12 UTC para que sea
%mas comparable con el experimento de SLAF.
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%PARAMETROS MODIFICABLES

modelo='HRMxx'  %Que modelo vamos a utilizar.
%Ruta del directorio donde estan los archivos
pathsuperensemble='D:/trabajos/TrabajoSLAF/precipitacion/superensemble/' %Pronosticos del superensemble.
pathcpc='D:/trabajos/TrabajoSLAF/precipitacion/datoscpc2006/'            %Datos de lluvia del CPC.

region=1;  %1 sur 2 norte.

%Genero el nombre del archivo de salida (en formato matlab). Y defino la
%region segun sea norte o sur.
if (region==1)
archivo_mat=strcat('D:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/',modelo,'_sur_gts.mat');
lat_n=-25;
lat_s=-45;
lon_e=-50;
lon_w=-80;
end
if (region==2)
archivo_mat=strcat('D:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/',modelo,'_norte_gts.mat');  
%Region tropical (menor densidad de datos).
lat_n=0;
lat_s=-20;
lon_e=-50;
lon_w=-80;
end

%Fecha de inicio y de fin del loop.
ini_date='01-Oct-2006';
end_date='30-Nov-2006';

%Paso la fecha de hoy a numero.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);

%*************************************************************************
%COMIENZA EL CICLO EN TIEMPO.
%*************************************************************************

precip24=[];
precip48=[];
while (date_num <= end_date_num)
    

    %Vamos a generar el nombre de los archivos que vamos a necesitar: 
    date_vec=datevec(date_num); %Genero un string con la fecha de hoy.
    fecha_hoy=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));
    date_vec=datevec(date_num-1); %Genero un string con la fecha de ayer.
    fecha_ayer=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));
    date_vec=datevec(date_num-2); %Genero un string con la fecha de aayer.
    fecha_aayer=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));
    
    cpcfile=strcat(pathcpc,'sa_12z.',fecha_hoy);
    f24file=strcat(pathsuperensemble,modelo,fecha_ayer(3:8),'12001.txt');
    f48file=strcat(pathsuperensemble,modelo,fecha_aayer(3:8),'12001.txt');
    
    date_vec=datevec(date_num);
    fecha_hoy_num=(date_vec(1)-2000)*1e8+date_vec(2)*1e6+date_vec(3)*1e4+12*1e2;
    date_vec=datevec(date_num-1);
    fecha_ayer_num=(date_vec(1)-2000)*1e8+date_vec(2)*1e6+date_vec(3)*1e4+12*1e2;
    %Fin de la generacion del nombre de los archivos.
    
    %Lectura de los archivos (cargamos la data en variables de matlab).
    
    if(fopen(cpcfile)~=-1);
    cpc=load(cpcfile);
    if(str2num(fecha_hoy)==cpc(1,1)) %Verifico si la fecha del nombre del archivo es la misma que la de los datos.
    %Lectura y filtrado de los datos del CPC.
    cpc(find(cpc==999))=0;
    cpc(find(squeeze(cpc(:,6))<0),6)=NaN;
    %Saco los undef (que ahora son NaN).
    i_aux=find(isnan(squeeze(cpc(:,6)))~=1 & cpc(:,6)< 300 & cpc(:,6) >=0 );
    aux(:,1:4)=cpc(i_aux,1:4);
    aux(:,5)=cpc(i_aux,6);
    clear cpc i_aux 
    cpc=aux;
    clear aux
    %Recorto la region
    aux=cpc(find(cpc(:,3)>=lat_s & cpc(:,3) <=lat_n),:);
    aux2=aux(find(aux(:,4)<=lon_e & aux(:,4) >=lon_w),:);
    clear cpc
    cpc=aux2;
    clear aux aux2
    
    
    %Fin de la lectura de los datos del CPC.
    
    if(fopen(f24file)~=-1);
    %Lectura y filtrado de los pronosticos a 24 horas.
    [auxdate1,auxest1,auxlat1,auxlon1,auxu1,auxv1,auxt1,auxtd1,auxp1,auxpp1] = textread(f24file,'%f%s%f%f%f%f%f%f%f%f');
    aux=str2double(auxest1); %Deja el numero en las que tienen numero y pone NaN en las otras que estan repetidas.
    i_aux=find(isnan(aux)==0 & auxdate1 <= fecha_hoy_num & auxdate1 >= fecha_ayer_num); %Reduzco al maximo el numero de datos para agilizar el proceso.
    aux24(:,1)=auxdate1(i_aux); %Genero un array con fecha,est,prono.
    aux24(:,2)=aux(i_aux);
    aux24(:,3)=auxpp1(i_aux);
    aux24(find(aux24==-999))=NaN;
    clear aux i_aux auxdate1 auxest1 auxlat1 auxlon1 auxu1 auxv1 auxt1 auxtd1 auxp1 auxpp1
    else
    aux24=NaN(1,3) %Si no encuentro el archivo entonces es todo NaN.
    end
    %Fin de la lecutra de los pronosticos a 24 horas.
    if(fopen(f48file)~=-1);
    %Lectura y filtrado de los pronosticos a 48 horas.
    [auxdate2,auxest2,auxlat2,auxlon2,auxu2,auxv2,auxt2,auxtd2,auxp2,auxpp2] = textread(f48file,'%f%s%f%f%f%f%f%f%f%f');
    aux=str2double(auxest2); %Deja el numero en las que tienen numero y pone NaN en las otras que estan repetidas.
    i_aux=find(isnan(aux)==0 & auxdate2 <= fecha_hoy_num & auxdate2 >= fecha_ayer_num); %Reduzco al maximo el numero de datos para agilizar el proceso.
    aux48(:,1)=auxdate2(i_aux); %Genero un array con fecha,est,prono.
    aux48(:,2)=aux(i_aux);
    aux48(:,3)=auxpp2(i_aux);
    aux48(find(aux48==-999))=NaN;
    clear aux i_aux auxdate2 auxest2 auxlat2 auxlon2 auxu2 auxv2 auxt2 auxtd2 auxp2 auxpp2
    else
    aux48=NaN(1,3);
    end
    %Fin de la lectura de los pronosticos a 48 horas.
    
    
    %Vamos a iniciar el loop sobre la estaciones para sumar la
    %precipitacion de cada modelo para las estaciones del archivo CPC.
    
    for i_est=1:length(cpc(:,1));
        
        aux(i_est,1:5)=cpc(i_est,1:5);
        
        aux2=aux24(find(aux24(:,2)==cpc(i_est,2)),3); %Busco todos los datos para esa estacion para el periodo seleccionado.
        i_aux=find(isnan(aux2)==0);
        if(length(i_aux)>0)
           a=length(aux2);
           if(a==4) %si los datos estan cada 6 horas
               isum=1:4;
               aux(i_est,6)=nansum(aux2(isum));
           else
           if(a==5)
               isum=2:5;
               aux(i_est,6)=nansum(aux2(isum));
           else
           if(a==9) %Si los datos estan cada 3 horas
               isum=3:2:9; %salteo 1 de por medio.
               aux(i_est,6)=nansum(aux2(isum));
           else
               aux(i_est,6)=NaN;
           end
           end
           end
 
        else
            aux(i_est,6)=NaN;
        end

        clear aux2 i_aux isum a
        
        aux3(i_est,1:5)=cpc(i_est,1:5);
        
        aux4=aux48(find(aux48(:,2)==cpc(i_est,2)),3); %Busco todos los datos para esa estacion para el periodo seleccionado.
        i_aux=find(isnan(aux4)==0);
        if(length(i_aux)>0)
           a=length(aux4);
           if(a==5) %si los datos estan cada 6 horas
               isum=2:5;
               aux3(i_est,6)=nansum(aux4(isum));
           else
           if(a==9) %Si los datos estan cada 3 horas
               isum=3:2:9; %salteo 1 de por medio.
               aux3(i_est,6)=nansum(aux4(isum));
           else
               aux3(i_est,6)=NaN;
           end
           end
            
        else
            aux3(i_est,6)=NaN;
        end

        clear aux4 i_aux isum a
        
    end %End del loop sobre las estaciones del CPC.
    precip24=[ precip24 ; aux ];
    precip48=[ precip48 ; aux3];
    clear aux aux3
    
    else
        display('no estan los datos')
        datevec(date_num)
    end %end del if sobre la fecha del archivo CPC.
    end %end del if sobre la existencia del archivo del CPC.
    
    clear aux48 aux24
    date_num=date_num+1
    %clear daily_data daily_field num_data %Si no borramos estas variables pueden quedar datos de dias anteriores.

end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!! 
    
    
%Guardamos todos los campos en un archivo .mat (la idea serÃ­a generar este
%archivo para todas las fuentes y tener todas las fuentes en el mismo
%formato en distintas resoluciones.


[ets hit_rate far]=ets_fun(precip24(:,5),precip24(:,6),[1 5 10 20 30 40 50])
 save(archivo_mat,'precip24','precip48');


 
 