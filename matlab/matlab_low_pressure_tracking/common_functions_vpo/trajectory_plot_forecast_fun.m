function trajectory_plot_forecast_fun(TrajectoryStruct,config)


%This function prepares the basic minimun information needed for the
%tracking routine.

ini_date_num=datenum(config.date_ini,'yyyymmddHH');
end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;
%This function will chek for premature splitings.

while ( cur_date_num <= end_date_num )

    %READ DATA...
    if(config.isforecast)
    MinFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(config.ensemblemember) '.bin']
    
    else    
    MinFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '.bin'];
    end
    
    tmpstruct=read_min_fun(MinFileName);
    
    

    mascara=zeros(tmpstruct.ny,tmpstruct.nx);
    for ii=1:tmpstruct.nminimos
        mascara(tmpstruct.minindex{ii})=ii;
    end
    fid=fopen('latinput.bin');
    lat=fread(fid,[180 91],'single')';
    fclose(fid);
    fid=fopen('loninput.bin');
    lon=fread(fid,[180 91],'single')';
    fclose(fid);
    
    figure
    pcolor(lon,flipdim(lat,1),mascara);
    shading flat
    hold on
    

    
    for itr=1:size(TrajectoryStruct,2)
       tmpind=find(TrajectoryStruct(itr).daten == cur_date_num);
       if(tmpind==1)
          if(length(TrajectoryStruct(itr).daten)==1)
              %Trayectoria de 1 solo punto
              plot(TrajectoryStruct(itr).minlon,TrajectoryStruct(itr).minlat,'ro');
          else
              %Trayectoria comenzando
              plot(TrajectoryStruct(itr).minlon(1:2),TrajectoryStruct(itr).minlat(1:2),'-go');
          end
           
       end
       if(tmpind > 1 & tmpind < length(TrajectoryStruct(itr).daten))
           %Trayectoria madura

           plot(TrajectoryStruct(itr).minlon(tmpind-1:tmpind+1),TrajectoryStruct(itr).minlat(tmpind-1:tmpind+1),'-bo');
       end
       if(tmpind > 1 & tmpind == length(TrajectoryStruct(itr).daten))
       %Trayectoria finalizando
          plot(TrajectoryStruct(itr).minlon(tmpind-1:tmpind),TrajectoryStruct(itr).minlat(tmpind-1:tmpind),'-ko');
       end 
    end

%======================================================================

%     %Define climatology file corresponding to the current date.
%     ClimFile=[config.clim_path '/' config.climprefix datestr(cur_date_num,config.climdateformat) config.climsufix];
%     %Define grib file corresponding to the current date.
%     %Define name for the file containing minimun information corresponding
%     %to the current date.
%     search='HGT:500'; 
%     %Define temporary output for decoded geopotential fields.
%     DataTmpFile='./tmpgribout.bin';
%     
%     GribFile=[config.data_path '/' datestr(ini_date_num,config.datadateformat) '/' config.dataprefix datestr(ini_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) config.datasufix];
%   
%     %Decode fields.
%     
%     GenLatLon=false;
% 
%     
%     fast_read_tigge_grib(GribFile,GenLatLon,DataTmpFile);
% 
%  
%     %Read geop data.
%     fid=fopen('tmpgribout.bin');
%     fread(fid,1,'single');
%     geo=fread(fid,[180 91],'single')';
%     fclose(fid);
%     fid=fopen(ClimFile);
%     clim=fread(fid,[91 180],'single');
%     fclose(fid);
%     
%     figure
%     pcolor(lon,flipdim(lat,1),flipdim(geo,1));
%     shading flat
%     figure
%     
%     pcolor(lon,flipdim(lat,1),flipdim(geo,1)-clim);
%     shading flat


    
count=count+1;   
cur_date_num=cur_date_num+config.timefrec/24;

end