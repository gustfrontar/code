

function [ radar , ref , dv , dadv , phidp , rhohv , kdp , present_ref , present_dv , present_dadv , present_phidp , present_rhohv , present_kdp ] = read_radar_netcdf(filename)


[present_ref]=test_netcdf_var(file,'dBZ');
[present_dv]=test_netcdf_var(file,'V');
[present_dadv]=test_netcdf_var(file,'VDA');
[present_phidp]=test_netcdf_var(file,'PhiDP');
[present_rhohv]=test_netcdf_var(file,'RhoHV');
[present_kdp]=test_netcdf_var(file,'KDP');


%Get dimensions
    elevation  = read_netcdf_var(filename,'elevation',false);
    azimuth    = read_netcdf_var(filename,'azimuth',false);
    range      = read_netcdf_var(filename,'range',false);
    time       = read_netcdf_var(filename,'time',false);

%Get Radar location
    radar.lon = read_netcdf_var(filename,'longitude',false);
    radar.lat = read_netcdf_var(filename,'latitude',false);
    radar.altitude = read_netcdf_var(filename,'altitude',false);
    
%Get Scan time 
    radar.time_coverage_start = read_netcdf_var(filename,'time_coverage_start',false)';
    radar.time_coverage_start([14 17])='_';
    radar.time_coverage_start=radar.time_coverage_start(1:20);
    
  
%Get Variable Data
  if( present_ref )
    input_ref = read_netcdf_var(filename,'dBZ',false);
  else
    input_ref = NaN( size(elevation) );
  end
  if( present_dv )
    input_dv = read_netcdf_var(filename,'V',false);
  else
    input_dv = NaN( size(elevation) );
  end
  if( present_phidp )
    input_phidp = read_netcdf_var(filename,'PhiDP',false);
  else
    input_phidp = NaN( size(elevation) );
  end
  if( present_kdp )
    input_kdp = read_netcdf_var(filename,'KDP',true);
  else
    input_kdp = NaN( size(elevation) );
  end
  if( present_rhohv )
    input_rhohv = read_netcdf_var(filename,'RhoHV',false);
  else
    input_rhohv = NaN( size(elevation) );
  end
  if( present_dadv )
    input_dadv = read_netcdf_var(filename,'VDA',false);
  else
    input_dadv = NaN( size(elevation) );
  end
    
    
    [ref , az , el]= order_variable(input_ref,azimuth,elevation) ;
    [dv , az , el] = order_variable(input_dv,azimuth,elevation) ;
    [phidp , az , el] = order_variable(input_phidp,azimuth,elevation) ;
    [kdp , az , el]= order_variable(input_kdp,azimuth,elevation) ;
    [rhohv , az , el] = order_variable(input_rhohv,azimuth,elevation) ;
    [dadv , az , el] = order_variable(input_dadv,azimuth,elevation) ;
    
    
    
    radar.ne=length(el);
    radar.na=size(az,1);
    radar.nr=length(range);
    
    radar.elevation=el;
    radar.azimuth=squeeze(mean(az,2));
    radar.range=range;
    radar.time=reshape(time,[radar.na radar.ne]);


    
    

    
    
    
end
    
    
    
    