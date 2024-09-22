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
ENDDATE='2007';

%%% NOMBRE DE LOS ARCHIVOS
PREFIJO='HGT500MONTHLYMEAN';
FORMATOFECHA='yyyymm';
SUFIJO='.grib2';

RES_TIGGE=2;   %Resolucion de los TIGGE en grados.

%%% GRILLA
NLAT=361;
NLON=720;


CLIMATOLOGIA=zeros(NLAT,NLON,12);
NDATA=zeros(12);

%%% PRIMERO LEO TODOS LOS MESES Y CALCULO LA MEDIA PARA CADA MES.

ANIOINI=str2num(INIDATE);
ANIOFIN=str2num(ENDDATE);
ANIOC=ANIOINI;

fprintf('Reading the data')
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
   if( exist(GRIBFILE)==2 && TmpFileInfo.bytes > 0);
   
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

%%%%%% TAKE THE MEAN.
for mes=1:12
   CLIMATOLOGIA(:,:,mes)=CLIMATOLOGIA(:,:,mes)./NDATA(mes);
end


%%%%% INTERPOLO HORIZONTALMENTE A LA RESOLUCION DE LOS DATOS TIGGE.
LAT=[INVENTARIO.lat_min:INVENTARIO.delta_lat:INVENTARIO.lat_max];
LON=[INVENTARIO.lon_min:INVENTARIO.delta_lon:INVENTARIO.lon_max];
[LON LAT]=meshgrid(LON,LAT);

LATTIGGE=[-90:RES_TIGGE:90];
LONTIGGE=[0:RES_TIGGE:360-RES_TIGGE];
[LONTIGGE LATTIGGE]=meshgrid(LONTIGGE,LATTIGGE);

%Verificar que luego del regrid el valor de los polos sea el mismo para
%todos los puntos que representan el polo.

CLIMATOLOGIATIGGE=NaN([size(LONTIGGE) 12]);

%for sobre los puntos i,j de la reticula de baja
  %busco los puntos que cumplen el criterio de caja
    %eventualmente trato los bordes este y oeste de forma preferencial.

[nyt nxt]=size(LATTIGGE);
%num_data=NaN(nyt,nxt);


fprintf('Performing horizontal interpolation');
for iy=1:nyt
    for ix=1:nxt
        
         %Defino los bordes de la caja considerando todas las
         %posibilidades.

         y_s=LATTIGGE(iy,ix)-(RES_TIGGE)/2;
         y_n=LATTIGGE(iy,ix)+(RES_TIGGE)/2;

         x_w=LONTIGGE(iy,ix)-(RES_TIGGE)/2;
         x_e=LONTIGGE(iy,ix)+(RES_TIGGE)/2;
         
        
         %Aplico promedio por cajas con condicion de borde ciclica en X no
         %tiene condicion de borde polar.
         %*****************************************************************
         indexinterp=find(  LAT < y_n & LAT >= y_s & (LON < x_e | LON > x_e + 360 ) & (LON >= x_w | LON <= x_w - 360 ));
         %num_data(iy,ix)=length(lut{iy,ix});
         %*****************************************************************
         for it=1:12
         tmp1=CLIMATOLOGIA(:,:,it);
         tmp2=tmp1(indexinterp);
         CLIMATOLOGIATIGGE(iy,ix,it)=mean(tmp2);
         end
         
         
    end
end

%%% FUERZO A QUE EN EL POLO, PARA TODOS LOS VALORES DE X TENGA EL MISMO
%%% VALOR DE ALTURA.
for it=1:12
CLIMATOLOGIATIGGE(1,:,it)=squeeze(mean(CLIMATOLOGIATIGGE(1,:,it)));
CLIMATOLOGIATIGGE(end,:,it)=squeeze(mean(CLIMATOLOGIATIGGE(end,:,it)));
end

%%%% LUEGO USO SPLINES CUBICOS PARA INTERPOLAR LAS MEDIAS MENSUALES A LAS
%%%% MEDIAS DIARIAS. PRIMERO CREO UN ARREGLO EN DONDE AGREGO UN MES ANTES
%%%% DE ENERO (DICIEMBRE) Y UN MES A CONTINUACION DE DICIEMBRE (ENERO).

AUXCLIM(:,:,1)=CLIMATOLOGIATIGGE(:,:,11);
AUXCLIM(:,:,2)=CLIMATOLOGIATIGGE(:,:,12);
AUXCLIM(:,:,3:14)=CLIMATOLOGIATIGGE;
AUXCLIM(:,:,15)=CLIMATOLOGIATIGGE(:,:,1);
AUXCLIM(:,:,16)=CLIMATOLOGIATIGGE(:,:,2);

%%% VAMOS A USAR UN ANIO DE REFERENCIA NO BISIESTO


%%% GENERO UN VECTOR
tmp=datenum('1979010100','yyyymmddHH');

DAILYVECTOR=1:365;   

%%% GENERO EL VECTOR DE REFERENCIA ASUMIENDO QUE LA MEDIA DE CADA MES
%%% REPRESENTA EL VALOR EN EL CENTRO DEL MES (DIA 15).


CLIMT(1)=datenum('19781115','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(2)=datenum('19781215','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(3)=datenum('19790115','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(4)=datenum('19790215','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(5)=datenum('19790315','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(6)=datenum('19790415','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(7)=datenum('19790515','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(8)=datenum('19790615','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(9)=datenum('19790715','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(10)=datenum('19790815','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(11)=datenum('19790915','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(12)=datenum('19791015','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(13)=datenum('19791115','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(14)=datenum('19791215','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(15)=datenum('19800115','yyyymmdd')-datenum('19790101','yyyymmdd');
CLIMT(16)=datenum('19800215','yyyymmdd')-datenum('19790101','yyyymmdd');

fprintf('Performing temporal interpolation');
DAILYCLIM=NaN(nyt,nxt,length(DAILYVECTOR));
for iy=1:nyt
    for ix=1:nxt
        DAILYCLIM(iy,ix,:)=interp1(CLIMT,squeeze(AUXCLIM(iy,ix,:)),DAILYVECTOR,'spline');
    end
end



%%% ESCRIBO LOS RESULTADOS. Agregar 29 de febrero.

for ii=1:size(DAILYCLIM,3)
    tmp=datenum('1979010100','yyyymmddHH')+ii-1;
    for jj=1:4

        fileout=['../RESULTS/CLIMATOLOGY/NCEP500HPA_2_CFSR/HGT' datestr(tmp,'mmddHH') '.grd' ];
        fid=fopen(fileout,'w');
        fwrite(fid,DAILYCLIM(:,:,ii),'single');
        fclose(fid);
        
        tmp=tmp+1/4;
    end
    
end

save('climatologia.mat','DAILYCLIM','LATTIGGE','LONTIGGE')


