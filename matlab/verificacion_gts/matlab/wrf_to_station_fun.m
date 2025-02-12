function [FORECAST ESTACIONES_UTILES]=wrf_to_station_fun(ESTACIONES,FECHAS,wrf_path,lat_wrf,lon_wrf,mask_wrf)
%*************************************************************************
%Este script lee los datos de las estaciones e interpola las salidas del
%WRF a dichos puntos.
%*************************************************************************
% Juan Ruiz - 2009
%*************************************************************************

endian='b';  %Endian del archivo WRF.
npronos=5;   %Cantidad de horas de pronostico para cada hora del dia.
nvariables=7;%Numero de variables que vamos a leer.
nhoras=4;    %Cantidad de horas del dia (00 06 12 18)
nest=length(ESTACIONES(:,1));
ntiempos=length(FECHAS);

[ny_w nx_w]=size(lat_wrf);


%Antes de iniciar el ciclo en tiempo determino para cada estacion cual es
%el punto mas cercano del WRF y elimino de las matrices de datos los puntos
%que estan fuera de la reticula (me fijo los que quedan lejos de cualquier
%punto de reticula).

[INDICE ESTACIONES_UTILES]=station_index_fun(ESTACIONES,lat_wrf,lon_wrf,mask_wrf,0.5);


nest=sum(ESTACIONES_UTILES);
%Defino las matrices
FORECAST=NaN(nest,ntiempos,nhoras,npronos,nvariables);

%COMIENZA EL CICLO EN TIEMPO.
for tiempo=1:ntiempos;
    
    date_num=datenum(num2str(FECHAS(tiempo)),'yyyymmddHH');

    for ihour=1:nhoras;
        date=datestr(date_num+(ihour-1)/4,'yyyymmddHH');
        for ihourf=1:npronos;
            if(ihour==1 | ihour==3) %Caso 00 o 12 UTC
                horasf{1}='00';horasf{2}='12';horasf{3}='24';horasf{4}='36';horasf{5}='48';
            end
            if(ihour==2 | ihour==4) %Caso 06 o 18 UTC
                horasf{1}='06';horasf{2}='18';horasf{3}='30';horasf{4}='42';horasf{5}='NaN';
            end
      
       %Leo la salida del WRF.
       file_name=strcat(wrf_path,'SURFACE',date,'d01F',horasf{ihourf},'.dat');

         %Leo el archivo de superficie correspondiente.
         [T TD U V SLP RAIN]=read_wrf_fun(file_name,nx_w,ny_w,endian);
        
       %Ahora me quedo solo con los puntos cercanos a las observaciones.

          FORECAST(:,tiempo,ihour,ihourf,1)=T(INDICE);
          FORECAST(:,tiempo,ihour,ihourf,2)=TD(INDICE);
          FORECAST(:,tiempo,ihour,ihourf,3)=U(INDICE);
          FORECAST(:,tiempo,ihour,ihourf,4)=V(INDICE);
          FORECAST(:,tiempo,ihour,ihourf,5)=SLP(INDICE);
          FORECAST(:,tiempo,ihour,ihourf,6)=sqrt(U(INDICE).^2+V(INDICE).^2);
          tmp=atan2(-U(INDICE),-V(INDICE))*180/pi;
          tmp(tmp < 0)=tmp(tmp<0)+360;          
          FORECAST(:,tiempo,ihour,ihourf,7)=tmp;
          
       end
                              
    end

end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


 
 

