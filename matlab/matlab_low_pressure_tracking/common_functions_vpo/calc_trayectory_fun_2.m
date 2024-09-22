
function [TrajStruct]=calc_trayectory_fun_2(config)
%Juan Ruiz 2010

%Esta funcion recibe datos en una estructura (input) y una serie de
%parametros en una estructura (config). Y a partir de eso calcula las
%trayectorias de sistemas.

%CONFIG ES UNA ESTRUCTURA QUE CONTIENE INFORMACION SOBRE LA FECHA,
%UBICACION DE LOS ARCHIVOS, NOMBRES DE LOS ARCHIVOS, ETC.

%==========================================================================
%LOOP SOBRE LOS TIEMPOS, EN ESTE LOOP SE LEEN LOS DATOS (UN TIEMPO A LA
%VEZ) Y SE IDENTIFICAN LOS SISTEMAS Y SUS CARACTERISTICAS PARA DICHO
%TIEMPO. 
%DICHA INFORMACION SE ALMANCENA EN UNA ESTRUCTURA QUE SE UTILIZA EN EL
%CALCULO DE LAS TRAYECTORIAS.
%==========================================================================

%ESTA VERSION DEL SCRIPT TRABAJA CON DATOS INTERPOLADOS A UNA RETICULA DE
%0.5X0.5 UTILIZANDO SPLINES CUBICOS PARA MEJOR LOCALIZACION DE LA POSICION
%DE LOS MINIMOS. TAMBIEN CALCULA LA INTENSIDAD DE LOS MINIMOS USANDO UNA
%APROXIMACION AL LAPLACIANO, PARA TENER UNA MEJOR NOCION DE CUALES MINIMOS
%VALE LA PENA SEGUIR (EN LUGAR DE USAR EL VALOR DE LA ANOMALIA PARA ESO QUE
%PUEDE NO SER TAN REPRESENTATIVO).


ini_date_num=datenum(config.date_ini,'yyyymmddHH');
end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;

tic
while ( cur_date_num <= end_date_num )

    
    %READ DATA...
    
    %ANALYSIS DATA.
    %TODO CORREGIR QUE PASA EN LOS ANIOS BISIESTOS.
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
    ClimFile=[config.clim_path '/' config.climprefix datestr(cur_date_num+adddate,config.climdateformat) config.climsufix];
    %Define temporary output for decoded geopotential fields.
    DataTmpFile='./tmpgribout.bin';
    
    if( ~config.isforecast)
    %Define grib file corresponding to the current date.
    InputFile=[config.data_path '/' config.dataprefix datestr(cur_date_num,config.datadateformat) config.datasufix];
  
    search='HGT:500'; 
    
    OutputFileName=[config.result_path '/MINIMOS_2/' datestr(ini_date_num,config.datadateformat) '.mat'];

    
    %Decode fields.
    if(strcmp(config.dataformat,'GRIB'))
    [tmp]=fast_read_grib_2(InputFile,ClimFile,search,DataTmpFile);
    MinStruct(count).daten=cur_date_num;
    MinStruct(count).nminimos=tmp.nminimos;
    MinStruct(count).minlat=tmp.minlat;
    MinStruct(count).minlon=tmp.minlon;
    MinStruct(count).minanom=tmp.minanom;
    MinStruct(count).minlap=tmp.minlap;
    MinStruct(count).linked=tmp.linked;
    MinStruct(count).linkedmin=tmp.linkedmin;
    MinStruct(count).minarea=tmp.minarea;
    MinStruct(count).meananom=tmp.meananom;
    
    elseif(strcmp(config.dataformat,'BIN'))
        
    [tmp]=fast_read_bin_2(InputFile,ClimFile);
    MinStruct(count).daten=cur_date_num;
    MinStruct(count).nminimos=tmp.nminimos;
    MinStruct(count).minlat=tmp.minlat;
    MinStruct(count).minlon=tmp.minlon;
    MinStruct(count).minanom=tmp.minanom;
    MinStruct(count).minlap=tmp.minlap;
    MinStruct(count).linked=tmp.linked;
    MinStruct(count).linkedmin=tmp.linkedmin;
    MinStruct(count).minarea=tmp.minarea;
    MinStruct(count).meananom=tmp.meananom;   
    end
    
    %Call system identification routines.
    
    else
    %Lets consider the case where it is a forecast.
    %Define grib file corresponding to the current date.
    GribFile=[config.data_path '/' datestr(ini_date_num,config.datadateformat) '/' config.dataprefix datestr(ini_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) config.datasufix];
  
    %Define name for the file containing minimun information corresponding
    %to the current date.

    %Decode fields.
  
    [tmp]=fast_read_tigge_grib_2(GribFile,ClimFile,DataTmpFile);
    MinStruct(count).daten=cur_date_num;
    MinStruct(count).nminimos=tmp.nminimos;
    MinStruct(count).minlat=tmp.minlat;
    MinStruct(count).minlon=tmp.minlon;
    MinStruct(count).minanom=tmp.minanom;
    MinStruct(count).minlap=tmp.minlap;
    MinStruct(count).linked=tmp.linked;
    MinStruct(count).linkedmin=tmp.linkedmin;
    MinStruct(count).minarea=tmp.minarea;
    MinStruct(count).meananom=tmp.meananom;
    
    OutputFileName=[config.result_path '/MINIMOS_2/' datestr(ini_date_num,config.datadateformat) '_M' num2str(int32(config.ensemblemember)) '.mat'];


%     fid=fopen(ClimFile)
%     %fread(fid,1,'single')
%     aclim=fread(fid,[91 180],'single');
%     
%     
%      fid=fopen(DataTmpFile);
%      fread(fid,1,'single')
%      a=fread(fid,[180 91],'single')';
%      figure
%      pcolor(flipdim(a,1))
%      fclose(fid)

    end

%======================================================================
    
count=count+1;   
cur_date_num=cur_date_num+config.timefrec/24;



  
end

save(OutputFileName,'MinStruct');   



tiempo=toc;
fprintf('TIME TO READ DATA=%f\n',tiempo)

%==========================================================================
%Calculamos las trayectorias intentando asociar los sistemas presentes en
%el tiempo T con aquellos presentes en el T+1
%==========================================================================
  tic
  [TrajStruct]=trayectory_fun_2(config,MinStruct);
  tiempo=toc;
  fprintf('TIME TO GET THE TRAJECTORIES=%f\n',tiempo)

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================



end








