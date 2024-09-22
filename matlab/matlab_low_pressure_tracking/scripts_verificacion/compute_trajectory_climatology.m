clear all
close all

ini_date='2007040112';
end_date='2010123112';
traj_path='../RESULTS/ANALISIS/TRAJECTORIES_2/';
out_file= '../RESULTS/ANALISIS/TRAJECTORIES_2/TRAJ_CLIM.mat';

MaxTime=29;
MinTime=1;

%ESTE SCRIPT CALCULA LA CLIMATOLOGIA DE LAS TRAYECTORIAS (PUNTOS DE INICIO,
%PUNTOS DE FIN, ETC).
%LA CLIMATOLOGIA LA VAMOS A CALCULAR SOBRE UNA RETICULA REGULAR DE
%RESOLUCION 10X10 (ES POCA RESOLUCION PERO SE DEBE A QUE TENEMOS POCOS
%ANIOS).

%Generamos la reticula de la climatologia.
resol_clim=10;
x_clim=0:resol_clim:360-resol_clim;
y_clim=-90:resol_clim:90;
[lon_clim lat_clim]=meshgrid(x_clim,y_clim);
[ny nx]=size(lon_clim);

%Generamos las diferentes categorias de largo (vamos a speparar el conteo
%de acuerdo con el largo de la trayectoria). En el caso de las trayectorias
%del reanalysis solo examino los tiempos 2:5 porque despues se repite lo
%mismo una y otra vez, con la diferencia que se altera el largo de algunas
%trayectorias.

length_clim=4:28;

%Separo tambien por los lead time (cuando se produjo el evento dentro de la
%ventana de 7 dias del pronostico, para los analysis uso la misma ventana
%de modo que los resultados sean comparables).

lead_time_clim=MinTime+1:MaxTime-1;

%Por cada matriz de conteo la siguiente estructura.
% X, Y, LARGO_TRAJ,LEAD_TIME DEL EVENTO, ESTACION DEL ANIO

CGENESYS=zeros(ny,nx,length(length_clim),length(lead_time_clim),4);
CLISIS=zeros(ny,nx,length(length_clim),length(lead_time_clim),4);
CMAXI=zeros(ny,nx,length(length_clim),length(lead_time_clim),4);
CDENS=zeros(ny,nx,length(length_clim),length(lead_time_clim),4);

%DEFINIMOS LOS DIAS JULIANOS DE LAS FECHAS DE CORTE.

sfall=datenum('1979032100','yyyymmddHH')-datenum('1979010100','yyyymmddHH');
swint=datenum('1979062100','yyyymmddHH')-datenum('1979010100','yyyymmddHH');
ssprg=datenum('1979092100','yyyymmddHH')-datenum('1979010100','yyyymmddHH');
ssumm=datenum('1979122100','yyyymmddHH')-datenum('1979010100','yyyymmddHH');

%COMENZAMOS EL CICLO PARA LEER LAS TRAYECTORIAS...

ini_date_num=datenum(ini_date,'yyyymmddHH');
end_date_num=datenum(end_date,'yyyymmddHH');

cdate=ini_date_num;

while ( cdate <= end_date_num)
    
    %Calculo el dia juliano para la fecha en cuestion.
    djul=cdate-datenum(datestr(cdate,'yyyy'),'yyyy');
    %Obtengo el indice correspondiente a la estacion del anio.
    if( djul > ssumm || djul < sfall)
        %Verano.
        si=1;
    elseif( djul > sfall && djul < swint)
        %Otonio
        si=2;
    elseif( djul > swint && djul < ssprg)
        %Invierno
        si=3;
    elseif( djul > ssprg && djul < ssumm)
        %Primavera
        si=4;
    end
    
    %Genero el nombre del archivo.
    file=[traj_path '/TRAJ_' datestr(cdate,'yyyymmddHH') '_L7.mat'];
    fprintf('PROCESSING DATE %s \n',datestr(cdate,'yyyymmddHH'));
    
    load(file);
    TrajStruct=AnalysisTrajStruct;
    
    ntraj=size(TrajStruct,2); %Obtengo la cantidad de trayectorias.
    
    %Voy a hacer un do sobre las trayectorias obteniendo la posicion de la
    %genesis, la lisis y su longitud. Pero voy a descartar todas las
    %trayectorias que esten presentes en el primer tiempo y en el ultimo
    %porque no tengo manera de conocer su longitud.
    
     for itraj=1:ntraj   
      ttimes=TrajStruct(itraj).leadtime/6+1;
      if(~isempty(ttimes) && ttimes(1) > MinTime && ttimes(end) < MaxTime )
      %La trayectoria no arranca en el primer tiempo ni termina en el ultimo.
      
      li=find(length_clim==length(ttimes));
      if(~isempty(li))
      %When and where is the genesys?
      minlon=TrajStruct(itraj).minlon(1);
      minlat=TrajStruct(itraj).minlat(1);
      ti=find(lead_time_clim==ttimes(1));
      xi=round(minlon/resol_clim)+1;
      if(xi>nx);xi=1;end
      if(xi<1);xi=nx;end
      yi=round((minlat+90)/resol_clim)+1;
      CGENESYS(yi,xi,li,ti,si)=CGENESYS(yi,xi,li,ti,si)+1;
      
      %When and where is the lisis?
      minlon=TrajStruct(itraj).minlon(end);
      minlat=TrajStruct(itraj).minlat(end);
      ti=find(lead_time_clim==ttimes(end));
      xi=round(minlon/resol_clim)+1;
      if(xi>nx);xi=1;end
      if(xi<1);xi=nx;end
      yi=round((minlat+90)/resol_clim)+1;
      CLISIS(yi,xi,li,ti,si)=CLISIS(yi,xi,li,ti,si)+1;
           
      %When and where is the peak intensity?
      [maxi mii]=max(TrajStruct(itraj).minlap);
      minlon=TrajStruct(itraj).minlon(mii);
      minlat=TrajStruct(itraj).minlat(mii);
      ti=find(lead_time_clim==ttimes(mii));
      xi=round(minlon/resol_clim)+1;
      if(xi>nx);xi=1;end
      if(xi<1);xi=nx;end
      yi=round((minlat+90)/resol_clim)+1;
      CMAXI(yi,xi,li,ti,si)=CMAXI(yi,xi,li,ti,si)+1;
      
      %Where is the cyclone located.
        for kk=1:length(ttimes)
         minlon=TrajStruct(itraj).minlon(kk);
         minlat=TrajStruct(itraj).minlat(kk);
         ti=find(lead_time_clim==ttimes(kk));
         xi=round(minlon/resol_clim)+1;
         if(xi>nx);xi=1;end
         if(xi<1);xi=nx;end
         yi=round((minlat+90)/resol_clim)+1;
         CDENS(yi,xi,li,ti,si)=CDENS(yi,xi,li,ti,si)+1;  
        end
    
      end
      end
     end
    
    
    cdate=cdate+1;
end

save(out_file,'CMAXI','CLISIS','CGENESYS','length_clim','lead_time_clim','lon_clim','lat_clim');


%==========================================================================
%ALGUNOS PLOTEOS DE PRUEBA PARA VER QUE TODO ESTE FUNCIONANDO BIEN.
%==========================================================================
load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end




%GRAFICO LAS GENESIS.
%TOMO LOS PRIMEROS 4 TIEMPOS DEL ANALYSIS.
coslat=cosd(lat_clim);

lead_index=(length_clim > 8);

tmp=sum(sum(sum(CGENESYS(:,:,lead_index,1:4,:),5),4),3);

figure
hold on
pcolor(lon_clim(2:end,:),lat_clim(2:end,:),tmp(2:end,:)./coslat(2:end,:));
plot(long,lat)



%GRAFICO LAS LISIS.
%TOMO LOS PRIMEROS 4 TIEMPOS DEL ANALYSIS.
coslat=cosd(lat_clim);

lead_index=(length_clim > 8);

tmp=sum(sum(sum(CLISIS(:,:,lead_index,end-3:end,:),5),4),3);

figure
hold on
pcolor(lon_clim(2:end,:),lat_clim(2:end,:),tmp(2:end,:)./coslat(2:end,:));
plot(long,lat)

%GRAFICO LA DENSIDAD DE LOS SISTEMAS.
%TOMO LOS PRIMEROS 4 TIEMPOS DEL ANALYSIS.
coslat=cosd(lat_clim);

lead_index=(length_clim > 8);

tmp=sum(sum(sum(CDENS(:,:,lead_index,end-3:end,:),5),4),3);

figure
hold on
pcolor(lon_clim(2:end,:),lat_clim(2:end,:),tmp(2:end,:)./coslat(2:end,:));
plot(long,lat)

