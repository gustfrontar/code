function [precip24,precip48,esta,lat,lon]=precip_model_fun(modelo,hora_inic,size24,size48,ini_date,end_date)
%*************************************************************************
%Argumentos de la funcion:
% model: string con el nombre del modelo. Se usa para identificar los
% archivos de texto y para generar el nombre de las salidas.
% region: 1 region sur, 2 region norte. Identifica que region vamos a
% considerar.
% hora_inic: A que hora se inicializa el modelo en cuestion 12 o 00 utc.
% size24: cuantos tiempos componen el pron�stico de lluvia a 24 horas, el
% archivo tiene salidas cada 3 horas, cada 6, esta presente el primer
% tiempo? de todo esto depende el n�mero de tiempos que tengo que sumar
% para obtener el acumulado en las primeras 24 horas.
% size48: Idem size 24 pero para el pron�stico a 48 horas.�
% ini_date: string con la fecha en formato 01-Oct-2006
% end_date: idem ini_date pero con la fecha de fin.
% source: Fuente de los datos, si es 1 es gts si es 2 es cmorph.

%*************************************************************************
%Este programa lee los datos del CPC y completa para cada dato con los
%pron�sticos de cada uno de los modelos.
%La idea es hacer un do sobre las fechas, para cada fecha abro el archivo
%del CPC correspondiente, verifico que est� bien la fecha de este archivo.
%Si es asi comienzo a leer las estaciones y busco las mismas estaciones en
%el archivo del modelo correspondiente y asi voy construyendo una matriz
%con observaciones y pronosticos que verifican al mismo tiempo de la
%observacion. No tomamos regiones por el momento. Si una estacion no esta
%en el pronostico se le asigna un NaN. Luego se trabajar� con aquellas
%estaciones que esten disponibles para todos los modelos.
%Por el momento usamos solamente pronosticos de las 12 UTC para que sea
%mas comparable con el experimento de SLAF.
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta del directorio donde estan los archivos
pathsuperensemble='/intercomparacion/current/' ;%Pronosticos del superensemble.

%Definimos como hacer la suma en funcion del valor de size 24 y size 48.
if (size24==5) %pronos cada 6 horas incluyendo el tiempo inicial
    isum24=2:5;
end
if (size24==4) %pronos cada 6 horas no incluyendo el tiempo inicial
    isum24=1:4;
end
if (size24==3) %pronos cada 12 horas incluyendo tiempo inicial
    isum24=2:3;
end
if (size24==2) %pronos cada 12 horas no incluyendo el tiempo inicial.
    isum24=1:2;
end
if (size24==9) %pronos cada 3 horas incluyendo el tiempo inicial
    isum24=3:2:9;
end
if (size48==3)
    isum48=2:3;
end
if (size48==5)
    isum48=2:5;
end
if (size48==9)
    isum48=3:2:9;
end

%Paso la fecha de hoy a numero.
ini_date_num=datenum(ini_date,'yyyymmdd');
date_num=datenum(ini_date,'yyyymmdd');
%date_num=datenum(ini_date);
%ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date,'yyyymmdd');
%end_date_num=datenum(end_date);

%*************************************************************************
%COMIENZA EL CICLO EN TIEMPO.
%*************************************************************************

precip24=[];
precip48=[];
while (date_num <= end_date_num)

    %Vamos a generar el nombre de los archivos que vamos a necesitar: 
    date_vec=datevec(date_num); %Genero un string con la fecha de hoy.
    fecha_hoy=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));
    date_vec=datevec(date_num+1); %Genero un string con la fecha de manana.
    fecha_man=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));
    date_vec=datevec(date_num+2); %Genero un string con la fecha de pasado manana.
    fecha_pman=num2str(date_vec(1)*10000+date_vec(2)*100+date_vec(3));

    estacion=load('-ascii', 'estac.txt');
    
    f24file=strcat(pathsuperensemble,modelo,fecha_hoy(3:8),hora_inic,'001.txt');
        
    date_vec=datevec(date_num);
    fecha_hoy_num=(date_vec(1)-2000)*1e8+date_vec(2)*1e6+date_vec(3)*1e4+12*1e2;
    date_vec=datevec(date_num+1);
    fecha_man_num=(date_vec(1)-2000)*1e8+date_vec(2)*1e6+date_vec(3)*1e4+12*1e2;
    date_vec=datevec(date_num+2);
    fecha_pman_num=(date_vec(1)-2000)*1e8+date_vec(2)*1e6+date_vec(3)*1e4+12*1e2;
    %Fin de la generacion del nombre de los archivos.
    
    %Lectura de los archivos (cargamos la data en variables de matlab).

    b=fopen(f24file);
    fclose all;
    if(b~=-1);
    %Lectura y filtrado de los pronosticos a 24 horas.
    [auxdate1,auxest1,auxlat1,auxlon1,auxu1,auxv1,auxt1,auxtd1,auxp1,auxpp1] = textread(f24file,'%f%s%f%f%f%f%f%f%f%f');
    aux=str2double(auxest1); %Deja el numero en las que tienen numero y pone NaN en las otras que estan repetidas.
    i_aux=find(isnan(aux)==0 & auxdate1 >= fecha_hoy_num & auxdate1 <= fecha_man_num); %Reduzco al maximo el numero de datos para agilizar el proceso.
    aux24(:,1)=auxdate1(i_aux); %Genero un array con fecha,est,prono.
    aux24(:,2)=aux(i_aux);
    aux24(:,3)=auxpp1(i_aux);
    aux24(find(aux24==-999))=NaN;
    clear aux i_aux auxdate1 auxest1 auxlat1 auxlon1 auxu1 auxv1 auxt1 auxtd1 auxp1 auxpp1
    else
    aux24=NaN(1,3); %Si no encuentro el archivo entonces es todo NaN.
    end
    %Fin de la lecutra de los pronosticos a 24 horas.
    b=fopen(f24file);
    fclose all;
    if(b~=-1);
    %Lectura y filtrado de los pronosticos a 48 horas.
    [auxdate2,auxest2,auxlat2,auxlon2,auxu2,auxv2,auxt2,auxtd2,auxp2,auxpp2] = textread(f24file,'%f%s%f%f%f%f%f%f%f%f');
    aux=str2double(auxest2); %Deja el numero en las que tienen numero y pone NaN en las otras que estan repetidas.
    i_aux=find(isnan(aux)==0 & auxdate2 >= fecha_man_num & auxdate2 <= fecha_pman_num); %Reduzco al maximo el numero de datos para agilizar el proceso.
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
    
    for i_est=1:length(estacion(:,1));
        
        aux(i_est,1)=estacion(i_est,1);
        
        aux2=aux24(find(aux24(:,2)==estacion(i_est,1)),3); %Busco todos los datos para esa estacion para el periodo seleccionado.
        i_aux=find(isnan(aux2)==0);
        if(length(i_aux)>0)
           a=length(aux2);
           if(a==size24)
               aux(i_est,2)=nansum(aux2(isum24));
           else
               aux(i_est,2)=NaN;
           end
 
        else
            aux(i_est,2)=NaN;
        end

        clear aux2 i_aux a
        
        aux3(i_est,1)=estacion(i_est,1);
        
        aux4=aux48(find(aux48(:,2)==estacion(i_est,1)),3); %Busco todos los datos para esa estacion para el periodo seleccionado.
        i_aux=find(isnan(aux4)==0);
        if(length(i_aux)>0)
           a=length(aux4);
           if(a==size48) %si los datos estan cada 6 horas
               isum=2:5;
               aux3(i_est,2)=nansum(aux4(isum48));
           else
               aux3(i_est,2)=NaN;
           end            
        else
            aux3(i_est,2)=NaN;
        end

        clear aux4 i_aux a
        
    end %End del loop sobre las estaciones del CPC.
    precip24=[aux(:,2)];
    precip48=[aux3(:,2)];
    clear aux aux3
        
    clear aux48 aux24
    date_num=date_num+1;
    %clear daily_data daily_field num_data %Si no borramos estas variables pueden quedar datos de dias anteriores.

end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!! 

esta=estacion(:,1);
lat=estacion(:,2);
lon=estacion(:,3);
