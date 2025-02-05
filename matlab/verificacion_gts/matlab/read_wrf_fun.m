function [T TD U10 V10 SLP RAIN TSLB SMOIS MCAPE Q2]=read_wrf_fun(file,nx_w,ny_w,endian)

%FUNCION PARA LEER LOS ARCHIVOS SURFACE DEL WRF 
%DEL CTL ASUMIMOS QUE ESTA ES EL ORDEN DE LAS VARIABLES.
%Q2             1  0  QV at 2 M (kg kg-1)
%T2             1  0  TEMP at 2 M (K)
%U10            1  0  U at 10 M (m s-1)
%V10            1  0  V at 10 M (m s-1)
%TSLB           4  0  SOIL TEMPERATURE (K)
%SMOIS          4  0  SOIL MOISTURE (m3 m-3)
%RAINC          1  0  ACCUMULATED TOTAL CUMULUS PRECIPITATION (mm)
%RAINNC         1  0  ACCUMULATED TOTAL GRID SCALE PRECIPITATION (mm)
%td2            1  0  Dewpoint Temperature at 2m (C)
%slp            1  0  Sea Levelp Pressure (hPa)
%mcape          1  0  MCAPE (J/kg)

%tot_vars=17;   %Numero total de variables (campos)
tot_vars=16;

narch=fopen(file,'r',endian);

var=NaN(ny_w,nx_w,tot_vars);



if(narch  ~= -1)
    fileinfo=dir(file);
    if( fileinfo.bytes > 0)
            %file
            for j=1:tot_vars
            %j
            var(:,:,j)=fread(narch,[nx_w ny_w],'single')';
            end

    end
    
    fclose(narch);

end

%Acomodamos lo que leimos a las diferentes variables segun lo que dice el
%CTL.

Q2=var(:,:,1);  %Humedad especifica a 2 metros (sin dimensiones)
T=var(:,:,2)-273.16;  %Temperatura a 2 metros (grados centigrados)
U10=var(:,:,3);       %Componente U del viento a 10 metros (m/s)
V10=var(:,:,4);       %Componetne V del viento a 10 metros (m/s)
TSLB=var(:,:,5:8);    %Temperatura del suelo a 4 profundidades diferentes
SMOIS=var(:,:,9:12);  %Humedad del suelo a 4 profundidades diferentes.
RAIN=var(:,:,13)+var(:,:,14);  %LLuvia acumulada desde el inicio del pronostico (mm)
TD=var(:,:,15);                %Temperatura de rocio a 2 metros (grados centigrados)
SLP=var(:,:,16);               %Presion reducida a nivel del mar (hpa)
%MCAPE=var(:,:,17);
MCAPE=NaN(nx_w,ny_w);          %CAPE de la parcela mas inestable. (J/kg)

