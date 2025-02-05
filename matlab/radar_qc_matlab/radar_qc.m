
clear all
close all

undef=-9999;

path='/home/jruiz/Dropbox/DATA/DATOS_RADAR/ANGUIL/20100111/120/';

files=dir([path '*.nc_dealiased']);

zthresh=5; %Threshold in dBZ for echo top computation.
snx=5;
sny=5;

minref=0;


error_ref=5; %Reflectivity error in DBZ.
error_dv=2;  %Doppler velocity error in m/s.

id_ref_obs=4001;
id_dv_obs=4002;

%Increments for cartesian grid (m).
dx=2000;
dz=2000;
maxz=25e3;  %Maximum height for the observations.

%QC
rhohvt=0.8; %Rhohv threshold for QC
echo_top_t=3000;%Echo top threshold (m).


radar_type=1;



for ifile=3:3 %size(files,1)
    
    filename=[ path files(ifile).name ];
    
    [radar , ref , dv , dvc , phidp , rhohv , kdp , present_ref , present_dv , present_dvc , present_phidp , present_kdp ]=read_radar_netcdf( filename );
    
    qcflag=zeros(size(ref));
    
    
    ref(ref < minref)=minref;
    
    %Georeference will be performed for each time due to possible changes
    %in observation strategy. 
    
    [ radar ]=georeference_radar_data( radar );
    
    radar.replacerefmissing=minref;
    
    radar.error_ref=error_ref;
    radar.error_dv =error_dv ;
    radar.id_ref_obs= id_ref_obs;
    radar.id_dv_obs = id_dv_obs;
    radar.radar_type=radar_type;
    radar.undef=undef;
    
    
    %Topography blocking.
    % TODO: En la version de python-fortran debe estar incorporado este
    % chequeo (importante sobre todo para radares como el de Cordoba).
    
    
    refc=ref;
    
    
    %Remove strongly attenuated beams
    [attenuation_factor]=compute_atenuation_qc(radar,ref);
    refc( attenuation_factor < 0.4 )=NaN;
    
    
    %Remove gates associated with small doppler velocity.
    if( present_dv )
   
    refc(abs(dv) < 1)=NaN;
    
    end
    
    
    %Remove gates with low echo top (clutter, PBL echoes)
    [echo_top_3d ,echo_depth_3d ,echo_depth_size_3d ,total_max_dbz_3d ,total_max_dbz_level_3d ,max_dbz_3d ,max_dbz_level_3d]= ...
    compute_echotop3d(radar,ref,zthresh,snx,sny);

    refc(echo_top_3d < echo_top_t & ref > 0 )=NaN;
    
    %Remove echoes associated with low RhoHV
    if( present_rhohv )
    
    rhohv(rhohv < 0)=NaN;
    [rhohvsmooth]=compute_boxmean(rhohv,1,1,1);
    
    refc( rhohvsmooth < rhohvt ) = NaN;
    
    end
    
    %Remove gates with low reflectivity below 3Km (PBL echoes)
    %refc( refc < 25 & radar.Z < 3e3 )=NaN;
    
    
    %Remove noise in the doppler velocity
    
    if( present_dvc )
        
    %Wind QC (Dealiasing is perfomed using pyart routines, but some faulty
    %points may remain which are cleaned with the following function).
    %Utilizar la rutina basada en el VAD para remover los puntos erroneos.
    dvc( dvc < -200) =NaN;
    dvc( dvc == 0 )= NaN;
    
    [dvcc , u_vad , v_vad , uo , vo , tmp_wind , vr_background , sample_coverage]=compute_wind_qc(radar,dvc,5,5,1,25,10);
    
    dvccc=dvcc;
    %Remove low doppler velocity values (possible clutter).
    dvccc(abs(dvccc) < 1)=NaN;
    
    end
    
    %Testear superobbing, dividir los archivos en conjuntos de 1 minuto.
    [ cart ] = define_cartesian_grid( radar , dx , dz , maxz) ;
    
    %[tmppath tmpfilename]=fileparts(filename); 
    
    date=radar.time_coverage_start([1:4  6:7 9:10]);
    hour=str2num(radar.time_coverage_start(12:13));   
    min=str2num(radar.time_coverage_start(15:16));
    sec=str2num(radar.time_coverage_start(18:19));
    
    radar.year=str2num(date(1:4));
    radar.month=str2num(date(5:6));
    radar.day=str2num(date(7:8));
    radar.hour=hour;
    radar.minute=min;
    radar.second=sec;
    
    radar.beam_wid_h=1;
    radar.beam_wid_v=1;
    
    radar.attenuation_factor=0;  %Global attenuation factor (i.e. rain over the radome)
    
    minelev=1;
    
    for ii=1:radar.ne  %Hago un loop sobre las elevaciones.
        if( ii > 1 )
          strhour=num2str(hour);
          if(hour < 10);strhour=['0' strhour];end
          strmin=num2str(min);
          if(min < 10 );strmin=['0' strmin];end
          %strsec=num2str(sec);
          %if(sec < 10 );strsec=['0' strsec];end
          
          fileout=[path '/ANGUIL2KM_LETKF_' date '_' strhour strmin '.dat']; 
          %I use the time corresponding to the first azimuth to decide if
          %this scan is going to be included or not in the current group.
          sec=sec + ( radar.time(1,ii) - radar.time(1,ii-1) );
          
        end
        if ( sec > 60 )
            
            endelev= ii - 1;
            
            tmp_radar=radar;
            tmp_radar.elevation=radar.elevation(minelev:endelev);
            tmp_radar.ne = ( endelev - minelev ) + 1;
            tmp_radar.Z=radar.Z(:,:,minelev:endelev);
            tmp_radar.longitude=radar.longitude(:,:,minelev:endelev);
            tmp_radar.latitude =radar.latitude(:,:,minelev:endelev);
            tmp_radar.Rh=radar.Rh(:,minelev:endelev);
            tmp_radar.height=radar.height(:,minelev:endelev);
            
            
            %fileout='tmp.bin';
            fileout
            [grid_ref , grid_count_ref , grid_dv , grid_count_dv , grid_az_ref , grid_el_ref , grid_ra_ref ]=radar_superobbing( tmp_radar , refc(:,:,minelev:endelev) , dvccc(:,:,minelev:endelev) , cart , fileout );
            
            minelev=endelev + 1 ;
            
            sec=sec-60;
            min = min + 1;
            if( min > 60 )
                hour=hour + 1;
                min=min-60;
            end
            if( hour > 23)
                hour=0;
                date=  datestr(datenum(date,'yyyymmdd')+1,'yyyymmdd');
            end
                
        end
        
   
    
    end
    
    
    %Write the results in a binary data structure (For analysis and
    %forecast verification).
    fileout=[path '/ANGUIL2KM_LETKF_' date '_' strhour strmin '.rad']; 
    write_radar_data_seq(radar,refc,dvccc,attenuation_factor,qcflag,fileout,'b');
    
 
end