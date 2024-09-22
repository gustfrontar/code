clear all
close all

%Este script calcula la climatologia diaria de una variable a partir de los
%reanalisis del NCEP. La idea seria para cada dia del anio tomar todos los
%dias comprendidos entre +/- 15 dias de todos los anios del reanalisis.
%Luego se promedian todos esos dias y se obtiene la media para ese dia en
%particular.

%La climatologia tambien va a servir para mostrar el ciclo diurno, ya que
%se hace una climatologia para cada una de las 4 horas en forma
%independiente.

%El output se guarda en un archivo por dia con el nombre de la fecha y en
%formato matlab.


%%%EL PATH DONDE ESTAN LOS DATOS.
PATHREA2='../DATA/CFSR/HGTCLIM/';

%%%EL TIPO DE GRIB. (1 o 2)
GRIBTYPE=2;

%%%LA VARIABLE QUE QUIERO OBTENER 
VARIABLE='HGT';

%%%EL NIVEL CORRESPONDIENTE
NIVEL='500';


%%% LA FECHA INICIAL DE LA CLIMATOLOGIA
INIDATE='1979';
%%% LA FECHA FINAL
ENDDATE='1979';

%%% NOMBRE DE LOS ARCHIVOS
PREFIJO='HGT500MONTHLYMEAN';
FORMATOFECHA='yyyymm';
SUFIJO='.grib2';

%%% GRILLA
NLAT=361;
NLON=720;


CLIMATOLOGIA=NaN(NLON,NLAT,12);

%%% PRIMERO LEO TODOS LOS MESES Y CALCULO LA MEDIA PARA CADA MES.

ANIOINI=str2num(INIDATE);
ANIOFIN=str2num(ENDDATE);
ANIOC=ANIOINI;

while (ANIOC <= ANIOFIN)

 %Obtengo la lista de dias (subindices de la matriz climatologia) que corresponden
 %al periodo comprendido entre la fecha actual - deltadias y la fecha

for mes=1:12

   if(mes < 10)
   GRIBFILE=[PATHREA2 PREFIJO num2str(ANIOC) '0' num2str(mes) SUFIJO];
   else
   GRIBFILE=[PATHREA2 PREFIJO num2str(ANIOC) num2str(mes) SUFIJO];  
   end
   
   %Chequeamos que el archivo exista y que no tenga tamanio 0.
   TmpFileInfo=dir(GRIBFILE);
   if( exist(GRIBFILE)==2 && TmpFileInfo.bytes > 0)
   
   %LEEMOS EL ARCHIVO GRIB.
   [DATA INVENTARIO] = read_grib(GRIBFILE,GRIBTYPE,[VARIABLE ':' NIVEL],'');
      
      %SUMO ESTE CAMPO PARA TODOS LOS DIAS EN LOS QUE TENDRA INFLUENCIA
      CLIMATOLOGIA(:,:,mes)=CLIMATOLOGIA(:,:,mes)+DATA;
      NDATA(mes)=NDATA(mes)+1;
      
   else
      
       display(['WARNING!!!!: EL ARCHIVO ' GRIBFILE ' NO FUE ENCONTRADO !!!'])
       
   end
  
end

ANIOC=ANIOC+1;
    
end




%%%% LUEGO USO SPLINES CUBICOS PARA INTERPOLAR LAS MEDIAS MENSUALES A LAS
%%%% MEDIAS DIARIAS.






%%% ESCRIBO LOS RESULTADOS.

% for ii=1:size(CLIMATOLOGIA,3)
%     tmp=datenum('1979010100','yyyymmddHH')+ii-1;
%     for jj=1:size(CLIMATOLOGIA,4)
% 
%         fileout=['../RESULTS/CLIMATOLOGY/NCEP500HPA/HGT' datestr(tmp,'mmddHH') '.grd' ];
%         fid=fopen(fileout,'w');
%         fwrite(fid,CLIMATOLOGIA(:,:,ii,jj),'single');
%         fclose(fid);
%         
%         tmp=tmp+1/4;
%     end
%     
% end

%save('climatologia.mat')


