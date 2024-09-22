clear all
close all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

%ESTE SCRIPT CALCULA LAS PROPIEDADES DEL ENSAMBLE A DISTINTOS TIEMPOS. LA
%IDEA ES GENERAR MATRICES QUE CONTENGAN PARA CADA TIEMPO Y UBICACION LA
%DISPERSION, LA MEDIA DEL ENSAMBLE, EL ERROR DEL ENSAMBLE.

%TODO: TODO, HAY QUE ADAPTAR LAS FUNCIONES QUE YA TENGO A ESTOS CALCULOS.

startdate='2008020112';     %Fecha inicial del calculo.
enddate  ='2008020212';     %Fecha final del calculo.
config.model='kwbc';
config.enssize=20;        %Cual es el numero maximo de miembros en el ensamble.

%CONFIGURATION.............................................................

config.leadtime=168;      %Requested lead time.      
config.data_path=['/media/GUSTFRONT/data/TIGGE/' config.model];
config.data_path=['/media/GUSTFRONT/data/TIGGE/' config.model '/SMALLGRIB/'];
config.result_path=['../RESULTS/' config.model '/'];
config.clim_path='../RESULTS/CLIMATOLOGY/NCEP500HPA_2/';
config.analysispath='../DATA/REA2NCEP/HGTVPO/';
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='GRIB';
config.gribversion=1;
config.dataprefix='HGT_500';
config.analysisdataprefix='REA2NCEP_HGT_';
config.datasufix='.grib';
config.datadateformat='yyyymmddHH';
config.isforecast=true;
config.timefrec=6;   
config.timebetweenforecast=24;           %Cada cuanto estan inicializados los pronosticos.

config.mintrajlength=12;                 %Minima longitud que debe tener una trayectoria para iniciar un grupo.

mkdir([config.result_path '/TRAJECTORIES/']);
mkdir([config.result_path '/MINIMOS/']);
mkdir([config.result_path '/GROUP/']);
mkdir([config.result_path '/ENSEMBLEATTRIBUTES/']);


%==========================================================================
% DEFINE GRIDS

analat=[-90:2.5:90];
analon=[0:2.5:357.5];
[analon analat]=meshgrid(analon,analat);

modlat=[-90:2:90];
modlon=[0:2:358];
[modlon modlat]=meshgrid(modlon,modlat);

%==========================================================================
% START LOOPS.

startdaten=datenum(startdate,'yyyymmddHH');
enddaten  =datenum(enddate  ,'yyyymmddHH');


totdates=enddaten-startdaten+1;
%ALLOCATE ARRAYS
forecastmean=NaN(size(modlat,1),size(modlat,2),totdates); %ALLOCATE FORECAST MEAN.
forecastmeananom=NaN(size(modlat,1),size(modlat,2),totdates);
forecastsprd=NaN(size(modlat,1),size(modlat,2),totdates); %ALLOCATE FORECAST SPRD.
forecastrank=NaN(size(modlat,1),size(modlat,2),totdates); %ALLOCATE FORECAST RANK.
errormean=NaN(size(modlat,1),size(modlat,2),totdates);    %ALLOCATE ENSEMBLE MEAN ERROR.
errorctrl=NaN(size(modlat,1),size(modlat,2),totdates);    %ALLOCATE CONTROL FORECAST ERROR. 
date=NaN(totdates);
%----------------

for ilead=0:config.timefrec:config.leadtime 
fprintf('PROCESANDO EL PLAZO=%f \n',ilead);
 
currentdate=startdaten;
    
itime=1;
while ( currentdate <= enddaten )
    
     
fprintf('PROCESANDO LA FECHA=%s \n',datestr(currentdate,'yyyymmddHH'));

    DataTmpFile='./tmpgribout.bin';
    
    %Read the analysis.
    AnaGribFile=[config.analysispath '/' config.analysisdataprefix datestr(currentdate+ilead/24,config.datadateformat) config.datasufix];
    search='HGT:500'; 
    fast_read_grib(AnaGribFile,search,false,DataTmpFile);     
    fid=fopen(DataTmpFile);
    fread(fid,1,'single');
    analysis=fread(fid,size(analat'),'single')';
    fclose(fid);
    %Remap analysis to the forecast grid.
    analysis=interp2(analon,analat,analysis,modlon,modlat,'linear');
    %Yrev
    analysis=flipdim(analysis,1);
    %-------------------
    
    %Read the climatology.
    tmpdate=datestr(currentdate,'mmdd');
    %The climatology is not define for the 29th of february. Use the 30th
    %march climatology instead.
    if(strcmp(tmpdate,'0229'));
        adddate=1;
    else
        adddate=0;
    end
    fid=fopen([config.clim_path '/HGT' datestr(currentdate+ilead/24+adddate,'mmddHH')  '.grd']);
    climatology=fread(fid,size(modlat),'single');


forecast=NaN(size(modlat,1),size(modlat,2),config.enssize);

for iens=1:config.enssize    

config.ensemblemember=iens;
  
    %Read the forecast.
    ModGribFile=[config.data_path '/' datestr(currentdate,config.datadateformat) '/' config.dataprefix datestr(currentdate,config.datadateformat) '_F' num2str(ilead) '_M' num2str(config.ensemblemember) config.datasufix];
    fast_read_tigge_grib(ModGribFile,false,DataTmpFile);
    fid=fopen(DataTmpFile);
    if(fid>0)
    fread(fid,1,'single');
    forecast(:,:,iens)=fread(fid,size(modlat'),'single')';
    fclose(fid);
    end
 
end
    %yrev
    forecast=flipdim(forecast,1);

%Calculamos las propiedades del ensamble.
forecastmean(:,:,itime)=mean(forecast,3);              %COMPUTE ENSEMBLE MEAN.
forecastsprd(:,:,itime)=std( forecast,[],3);           %COMPUTE SPREAD.
tmp=repmat(analysis,[1 1 config.enssize]);
forecastrank(:,:,itime)=sum(forecast < tmp , 3)+1;     %COMPUTE RANK.
date(itime)=currentdate;
forecastmeananom(:,:,itime)=forecastmean(:,:,itime)-climatology;

%Calculamos el error del pronostico.
errormean(:,:,itime)=forecastmean(:,:,itime)-analysis; %COMPUTE ENSEMBLE MEAN ERROR.
errorctrl(:,:,itime)=forecast(:,:,1)-analysis;         %COMPUTE CONTROL FORECAST ERROR.

currentdate=currentdate+config.timebetweenforecast/24;

itime=itime+1;

end

%GUARDO LO QUE LEI Y CALCULE EN UN ARCHIVO.

save([config.result_path '/ENSEMBLEATTRIBUTES/ATTRIBUTES_F' num2str(ilead) '_' datestr(startdaten,config.datadateformat) '_' datestr(enddaten,config.datadateformat) '.mat']);


end

 
 