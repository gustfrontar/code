close all
clear all

%LA IDEA DE ESTE SCRIPT ES SELECCIONAR UNA FECHA, UN ENSAMBLE Y UN GRUPO
%DETERMINADO Y GENERAR UNA SERIE DE PLOTS QUE INCLUYAN LAS MASCARAS, LAS
%ANOMALIAS Y LAS TRAYECTORIAS ASOCIADAS A CADA GRUPO PARA ENTENDER MEJOR LA
%EVOLUCION EN CASOS PARTICULARES Y PODER ENTENDER MEJOR COMO ESTA
%FUNCIONANDO LA METODLOGIA DE TRACKING.

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

%LOS MAS IMPORTANTES PARA ESTE SCRIPT.
config.date_ini='2008052512';          
config.group=47;
config.model='ecmf';
config.enssize=20;

%LOS QUE NO CAMBIAN GENERALMENTE
config.datadateformat='yyyymmddHH';
config.resultfrec=24; 
config.grouppath=['../RESULTS/' config.model '/GROUP_2/'];
%config.minpath=['../RESULTS/' config.model '/MINIMOS/'];
config.forecastlength=7;
config.leadtime=168;      %Requested lead time.      
config.data_pathfor=['/home/jruiz/TIGGE/' config.model '/SMALLGRIB/'];
config.data_pathana=['../DATA/CFSR/HGTVPO/'];
config.result_pathana=['../RESULTS/ANALISISCFSR/'];
config.result_pathfor=['../RESULTS/' config.model '/'];
config.clim_pathana='../RESULTS/CLIMATOLOGY/NCEP500HPA_2_CFSR/';
config.clim_pathfor='../RESULTS/CLIMATOLOGY/NCEP500HPA_2_CFSR/';
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='GRIB';
config.gribversion=1;
config.dataprefix='HGT_500';
config.datasufix='.grib';
config.datadateformat='yyyymmddHH';
config.isforecast=true;
config.timefrec=6;   
config.timebetweenforecast=24; %Cada cuanto estan inicializados los pronosticos.
config.mintrajlength=12;       %Minima longitud que debe tener una trayectoria para iniciar un grupo.

%CARGO EL ARCHIVO CON LOS GRUPOS.
load_file=['../RESULTS/' config.model '/GROUP_2/GROUP_' config.date_ini '_L' num2str(config.forecastlength) '.mat'];
load(load_file);

%DEFINIMOS LAT Y LON

lat_analysis=-90:2:90;
lon_analysis=0:2:358;

[lon_analysis lat_analysis]=meshgrid(lon_analysis,lat_analysis);

lat_forecast=-90:2:90;
lon_forecast=0:2:358;
[lon_forecast lat_forecast]=meshgrid(lon_forecast,lat_forecast);


%PRIMERO TENGO QUE LEER PARA TODO EL PRONOSTICO, EL ANALISIS Y LOS
%PRONOSTICOS DEL ENSAMBLE.

config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+config.forecastlength,'yyyymmddHH');

ini_date_num=datenum(config.date_ini,'yyyymmddHH');
end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;



while ( cur_date_num <= end_date_num )

    %READ DATA...
    
    %ANALYSIS DATA.
    %======================================================================
    tmpdate=datestr(cur_date_num,'mmdd');
    %The climatology is not define for the 29th of february. Use the 30th
    %march climatology instead.
    if(strcmp(tmpdate,'0229'));
        adddate=1;
    else
        adddate=0;
    end
    
    %Define climatology file corresponding to the current date.
    ClimFile=[config.clim_pathana '/HGT'  datestr(cur_date_num+adddate,config.climdateformat) config.climsufix];
    %Define temporary output for decoded geopotential fields.
    DataTmpFile='./tmpgribout.bin';
    
    %PRIMERO LEO EL ANALYISIS.
    %Define grib file corresponding to the current date.
    BinFile=[config.data_pathana '/CFSR_HGT_' datestr(cur_date_num,config.datadateformat) '.bin'];
   
    %Decode fields.
    %fast_read_grib(GribFile,search,true,DataTmpFile);
    %Call system identification routines.
   
     fid=fopen(ClimFile);
     climatology_analysis(:,:,count)=fread(fid,[91 180],'single');
     fclose(fid);
     fid=fopen(BinFile);
     %fread(fid,1,'single');
     geopotencial_analysis(:,:,count)=fread(fid,[91 180],'single');
     %geopotencial_analysis(:,:,count)=flipdim(tmp,1);
    
    %LEO LOS MINIMOS DEL ANALYSIS.
    load([config.result_pathana '/MINIMOS_2/' datestr(ini_date_num,config.datadateformat) '.mat']);
    %tmpminstruct=read_min_fun(MinFileName);
    
    %Convierto la estructura de los minimos a mascara.
    %tmpmask=zeros(73,144);
    %for jj=1:tmpminstruct.nminimos
    %    tmpindex=tmpminstruct.minindex{jj};
    %    tmpmask(tmpindex)=jj;
    %end
    %mascara_analysis(:,:,count)=tmpmask;
    minlatanalysis{count}=MinStruct(count).minlat;
    minlonanalysis{count}=MinStruct(count).minlon;
    
    

    ClimFile=[config.clim_pathfor '/HGT'  datestr(cur_date_num+adddate,config.climdateformat) config.climsufix]; 
    %Lets consider the case where it is a forecast.
    %Define grib file corresponding to the current date.
    %LEO EL ARCHIVO CON LOS MINIMOS.
    fid=fopen(ClimFile);
    climatology_forecast(:,:,count)=fread(fid,[91 180],'single');
    fclose(fid);
     
     for ii=1:config.enssize;
     config.ensemblemember=ii-1;
     GribFile=[config.data_pathfor '/' datestr(ini_date_num,config.datadateformat) '/HGT_500'  datestr(ini_date_num,config.datadateformat)...
         '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) config.datasufix];
     %Decode fields.
     GenLatLon=false;
     read_error=fast_read_tigge_grib(GribFile,GenLatLon,DataTmpFile)
     
     fid=fopen(DataTmpFile);
     fread(fid,1,'single');
     tmp=fread(fid,[180 91],'single')';
     geopotencial_forecast(:,:,ii,count)=flipdim(tmp,1);
     fclose(fid);
     
     %LEO TAMBIEN LOS ARCHIVOS CON LOS MINIMOS.  
     load([config.result_pathfor '/MINIMOS_2/' datestr(ini_date_num,config.datadateformat) '_M' num2str(int32(config.ensemblemember)) '.mat']);  
     %tmpminstruct=read_min_fun(MinFileName);
     %Convierto la estructura de los minimos a mascara.
     %if(count ==1 && ii==12)
     %tmpminstruct.nminimos
     %auxiliartmp=tmpminstruct;
     %end
     %tmpmask=zeros(91,180);
     % for jj=1:tmpminstruct.nminimos
     %    tmpindex=tmpminstruct.minindex{jj};
     %    tmpmask(tmpindex)=jj;
     % end
     % mascara_forecast(:,:,ii,count)=tmpmask;
     minlatforecast{ii,count}=MinStruct(count).minlat;
     minlonforecast{ii,count}=MinStruct(count).minlon;
     end
     

  
%CALCULO LAS ANOMALIAS A PARTIR DE LOS CAMPOS DE GEOPOTENCIAL QUE LEI.     
anomaly_analysis(:,:,count)=geopotencial_analysis(:,:,count)-climatology_analysis(:,:,count);
for jj=1:config.enssize;
anomaly_forecast(:,:,jj,count)=geopotencial_forecast(:,:,jj,count)-climatology_forecast(:,:,count);
end


cur_date_num=cur_date_num+6/24;
 count=count+1;
    
end

%TERMINE DE LEER TODO! ACA TENGO LAS ANOMALIAS PARA LOS MIEMBROS DEL
%ENSAMBLE Y LOS ANALISIS, LAS MASCARAS Y LOS GRUPOS QUE A SU VEZ TIENEN
%TODAS LAS TRAYECTORIAS.

%AHORA LA IDEA SERIA HACER PLOTS QUE SIRVAN DE DIAGNOSTICO PARA ENTENDER
%CUAL ES EN UN CASO PARTICULAR LA RAZON POR LA QUE UN GRUPO DETERMINADO SE
%COMPORTA DE LA MANERA QUE SE COMPORTA. Y EN QUE MEDIDA EL COMPORTAMIENTO
%DEL GRUPO REPRESENTA LO QUE QUEREMOS ANALIZAR QUE ES LA EVOLUCION DE LAS
%VAGUADAS EN EL CAMPO DE 500 HPA.


%UN POSIBLE PLOTEO SERIA GRAFICAR PARA LOS X PLAZOS DE PRONOSTICO UN
%GRAFICO CON TODOS LOS MIEMBROS DEL ENSAMBLE Y EL ANALISIS EN DONDE SE
%MUESTRE EN CADA PANEL UN MIEMBRO DEL ENSAMBLE, LA MASCARA DE FONDO, LA
%ANOMALIA EN CONTORNOS Y UN CONTORNO INDICANDO LA POSICION DE LA
%TRAYECTORIA Y LA POSICION DEL MINIMO EN EL TIEMPO DADO Y EN EL ANTERIOR Y
%EL SIGUIENTE.

%PRIMERO OBTENEMOS EL RANGO DE TIEMPOS DEL GRUPO.

%OBTENGO DONDE EMPIEZA Y DONDE TERMINA EL GRUPO PARA LOS PRONOSTICOS
%(USANDO LA MEDIA) Y PARA EL ANALYSIS USANDO EL ANALYSIS.
inicio_mean=min(find(~isnan(group.meanlat(:,config.group))));
fin_mean=max(find(~isnan(group.meanlat(:,config.group))));

inicio_analysis=min(find(~isnan(group.minlatanalysis(:,config.group))));
fin_analysis=max(find(~isnan(group.minlatanalysis(:,config.group))));

tini=min([inicio_mean inicio_analysis]);
tfin=max([fin_mean    fin_analysis   ]);


ncols=ceil(sqrt(config.enssize+1));  %Calculo la cantidad de filas y columnas de los subplot.

for itime=tini:tfin


latcent=group.meanlat(itime,config.group);
loncent=group.meanlon(itime,config.group);

if(isnan(latcent))
    latcent=group.minlatanalysis(itime,config.group);
    loncent=group.minlonanalysis(itime,config.group);
end


deltalat=20;
deltalon=20;

figure
subplot(ncols,ncols,1)  %Aca viene el ploteo del analysis.
hold on
%pcolor(lon_analysis,lat_analysis,mascara_analysis(:,:,itime));
shading flat
contour(lon_analysis,lat_analysis,anomaly_analysis(:,:,itime),'k-','LineWidth',1.5);
plot(group.minlonanalysis(:,config.group),group.minlatanalysis(:,config.group),'bo-');
plot(minlonanalysis{itime},minlatanalysis{itime},'gv','MarkerSize',12);
plot(group.minlonanalysis(itime,config.group),group.minlatanalysis(itime,config.group),'ro','LineWidth',2);
plot(group.meanlon(itime,config.group),group.meanlat(itime,config.group),'ko','LineWidth',2);
axis([loncent-deltalon loncent+deltalon latcent-deltalat latcent+deltalat])

title('Analysis')

for ii=1:config.enssize
subplot(ncols,ncols,ii+1) 
hold on
%pcolor(lon_analysis,lat_analysis,mascara_analysis(:,:,itime));
shading flat
contour(lon_forecast,lat_forecast,anomaly_forecast(:,:,ii,itime),'k-','LineWidth',1.5);
plot(group.minlon(ii,:,config.group),group.minlat(ii,:,config.group),'bo-','LineWidth',2);
plot(minlonforecast{ii,itime},minlatforecast{ii,itime},'gv','MarkerSize',12);
plot(group.minlon(ii,itime,config.group),group.minlat(ii,itime,config.group),'ro','LineWidth',2);
plot(group.meanlon(itime,config.group),group.meanlat(itime,config.group),'ko','LineWidth',2);
axis([loncent-deltalon loncent+deltalon latcent-deltalat latcent+deltalat])
title(['Member ' num2str(ii)]) 
end


end







