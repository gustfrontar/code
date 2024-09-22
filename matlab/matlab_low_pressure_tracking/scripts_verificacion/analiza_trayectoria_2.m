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
config.trajectory=226;
config.model='ANALISISCFSR';


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
load_file=['../RESULTS/' config.model '/TRAJECTORIES_2/TRAJ_' config.date_ini '_L' num2str(config.forecastlength) '.mat'];
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
     
    
     

  
%CALCULO LAS ANOMALIAS A PARTIR DE LOS CAMPOS DE GEOPOTENCIAL QUE LEI.     
anomaly_analysis(:,:,count)=geopotencial_analysis(:,:,count)-climatology_analysis(:,:,count);


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

tini=min(AnalysisTrajStruct(config.trajectory).time);
tfin=max(AnalysisTrajStruct(config.trajectory).time);
tfinori=tfin;

count=1;
if(tfin < size(anomaly_analysis,3)-3)
    tfin=tfin+3;
end
    
latcent=mean(AnalysisTrajStruct(config.trajectory).minlat);
loncent=mean(AnalysisTrajStruct(config.trajectory).minlon);
deltalat=20;
deltalon=50;

for itime=tini:tfin



figure
hold on
%pcolor(lon_analysis,lat_analysis,mascara_analysis(:,:,itime));
shading flat
contour(lon_analysis,lat_analysis,anomaly_analysis(:,:,itime),[-300:20:300],'k-','LineWidth',1.5);
plot(minlonanalysis{itime},minlatanalysis{itime},'gv','MarkerSize',12);
plot(AnalysisTrajStruct(config.trajectory).minlon,AnalysisTrajStruct(config.trajectory).minlat,'ro-','LineWidth',2);
axis([loncent-deltalon loncent+deltalon latcent-deltalat latcent+deltalat])

title('Analysis')


    


count=count+1;
end


fclose all





