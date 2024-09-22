
function [TrajStruct,error_string]=calc_trayectory_fun_2(config)
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
    GribFile=[config.data_path '/' config.dataprefix datestr(cur_date_num,config.datadateformat) config.datasufix];
  

    %Define name for the file containing minimun information corresponding to the current date.
    OutputFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '.bin'];
    %Define search criteria in the grib inventory.
    search='HGT:500'; 
    
    %Decode fields.
    fast_read_grib_2(GribFile,search,DataTmpFile);
    %Call system identification routines.
    
    %CALL THE PROGRAM FIND MINIMUN (FORTRAN) TO GENERATE THE MINIMUN INFORMATION FILE CORRESPONDING TO THE CURRENT DATE.    
    find_minimun_laplaciano_global_f90(DataTmpFile,'./DataLatGribOut.bin','./DataLonGribOut.bin',ClimFile,'./',OutputFileName);
 
    
    else
    %Lets consider the case where it is a forecast.
    %Define grib file corresponding to the current date.
    GribFile=[config.data_path '/' datestr(ini_date_num,config.datadateformat) '/' config.dataprefix datestr(ini_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) config.datasufix];
  
    %Define name for the file containing minimun information corresponding
    %to the current date.
    OutputFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) '.bin'];
     
    %Decode fields.
  
    read_error=fast_read_tigge_grib_2(GribFile,true,DataTmpFile);

    if(read_error~=0)
        TrajStruct=[];
        error_string='ERROR READING THE INPUT GRIB';
        return  %If an error occours during reading do not continue with the execution of the script.
    end
    %Call system identification routines.   
    %CALL THE PROGRAM FIND MINIMUN (FORTRAN) TO GENERATE THE MINIMUN INFORMATION FILE CORRESPONDING TO THE CURRENT DATE.    
    find_minimun_laplaciano_global_forecast_f90(DataTmpFile,'./DataLatGribOut.bin','./DataLonGribOut.bin',ClimFile,'./',OutputFileName);

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



tiempo=toc;
fprintf('TIME TO READ DATA=%f\n',tiempo)

%==========================================================================
%Calculamos las trayectorias intentando asociar los sistemas presentes en
%el tiempo T con aquellos presentes en el T+1
%==========================================================================
  tic
  [TrajStruct]=trayectory_fun(config);
  tiempo=toc;
  fprintf('TIME TO GET THE TRAJECTORIES=%f\n',tiempo)

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================



end








