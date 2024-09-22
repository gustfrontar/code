clear all
close all

%Este script lee la climatologia diaria-horaria derivada a partir de los
%reanalysis del NCEP y obtiene la misma en una reticula diferente de
%resolucion similar o mayor. 
%La reticula final es regular.

%ESTA VERSION REMUEVE EL CICLO DIURNO DE LA CLIMATOLOGIA... ESO NO SIRVIO
%PARA NADA Y LO UNICO QUE HACE ES METER RUIDO .



%%%LA VARIABLE QUE QUIERO OBTENER 
VARIABLE='HGT';

%%%EL NIVEL CORRESPONDIENTE
NIVEL='500';


%%% NOMBRE DE LOS ARCHIVOS

RES_ORIGINAL=2.5;
RES_FINAL=2;
CLIMPATH=['../RESULTS/CLIMATOLOGY/NCEP' NIVEL 'HPA_' num2str(RES_ORIGINAL) '/'];
NEWPATH=['../RESULTS/CLIMATOLOGY/NCEP' NIVEL 'HPA_' num2str(RES_FINAL) '_NDC/'];
NEWPATH2=['../RESULTS/CLIMATOLOGY/NCEP' NIVEL 'HPA_' num2str(RES_ORIGINAL) '_NDC/'];
mkdir(NEWPATH);
mkdir(NEWPATH2);


%%% GRILLA
NLAT=73;
NLON=144;


%%% LEO LA CLIMATOLOGIA.

CLIMATOLOGIA=zeros(NLAT,NLON,365,4);  %NLAT, NLON, 365 DAYS AND 4 HOURS 00, 06, 12 and 18.

for ii=1:size(CLIMATOLOGIA,3)
    tmp=datenum('1979010100','yyyymmddHH')+ii-1;
    for jj=1:size(CLIMATOLOGIA,4)

        file=[CLIMPATH '/' VARIABLE datestr(tmp,'mmddHH') '.grd' ];
        fid=fopen(file);
        CLIMATOLOGIA(:,:,ii,jj)=fread(fid,[NLAT NLON],'single');
        fclose(fid);
        
        tmp=tmp+1/4;
    end
    
end

%REESCRIBO LA CLIMATOLOGIA EN UN NUEVO ARCHIVO.

LAT=[-90:RES_ORIGINAL:90];
LON=[0:RES_ORIGINAL:360-RES_ORIGINAL];
[LON LAT]=meshgrid(LON,LAT);

LATNEW=[-90:RES_FINAL:90];
LONNEW=[0:RES_FINAL:360-RES_FINAL];
[LONNEW LATNEW]=meshgrid(LONNEW,LATNEW);


%PROMEDIO EL CICLO DIURNO PARA REMOVERLO TOTALMENTE
CLIMATOLOGIA=mean(CLIMATOLOGIA,4);

for ii=1:size(CLIMATOLOGIA,3)
    tmp=datenum('1979010100','yyyymmddHH')+ii-1;
    
    tmpfield=CLIMATOLOGIA(:,:,ii);
    tmpfieldint=interp2(LON,LAT,tmpfield,LONNEW,LATNEW,'spline');
    
    for jj=1:4
        %MANTENEMOS EL FORMATO DE 4 ARCHIVOS POR DIA PARA NO TENER QUE
        %CAMBIAR TODO EL RESTO, PERO LOS 4 ARCHIVOS VAN A SER IGUALES.

        file=[NEWPATH '/' VARIABLE datestr(tmp,'mmddHH') '.grd' ];
        fid=fopen(file,'w');

        
        fwrite(fid,tmpfieldint,'single');
        fclose(fid);
        
        file=[NEWPATH2 '/' VARIABLE datestr(tmp,'mmddHH') '.grd' ];
        fid=fopen(file,'w');

        
        fwrite(fid,tmpfield,'single');
        fclose(fid);
        
        tmp=tmp+1/4;
    end
    
end

%LISTO EL POLLO!

