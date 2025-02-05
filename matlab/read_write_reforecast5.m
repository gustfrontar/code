%Programa par leer reforecast


clear all;
close all;

tiempos=61 %(2 meses de datos)
pert=15    %(7 pares + 1 control)

%Definimos la zona norte

latminn=-20;
latmaxn=0;
lonminn=280;
lonmaxn=310;

%Definimos la zona sur

latmins=-45;
latmaxs=-30;
lonmins=280;
lonmaxs=310;

%Definimos con que variable vamos a trabajar y las latitudes y longitudes de la reticular correspondiente.

plot_var=char('vwnd');
lat_var=char('lat');
lon_var=char('lon');
ensemble_var=char('ensemble_member');

filein='/home/juan/reforecast/vwnd850_members_12_12_ind.nc';

wrfin=netcdf(filein,'r');

bin = fopen('/home/juan/reforecast/vwnd850_members_12_12_ind.bin','w');


%Cargamos la variable y la lat-lon seleccionada y la mascara

variables(1).values=wrfin{plot_var}(:);
variables(1).description=wrfin{plot_var}.long_name(:);
variables(1).units=wrfin{plot_var}.units(:);
variables(1).offset=wrfin{plot_var}.add_offset(:);
variables(1).factor=wrfin{plot_var}.scale_factor(:);

variables(2).values=wrfin{lat_var}(:);
variables(2).description=wrfin{lat_var}.long_name(:);
variables(2).units=wrfin{lat_var}.units(:);

variables(3).values=wrfin{lon_var}(:);
variables(3).description=wrfin{lon_var}.long_name(:);
variables(3).units=wrfin{lon_var}.units(:);

variables(4).values=wrfin{ensemble_var}(:);  %Nos dice a que miembro corresponde cada campo. -1 n1, 1 p1 y asi.


%Calculo el rango de latitudes y longitudes abarcado por el dominio.

minlat=min(min(variables(2).values));
minlon=min(min(variables(3).values));
maxlat=max(max(variables(2).values));
maxlon=max(max(variables(3).values));

%Obtengo los valores de la latitud y la longitud.

lat_p=variables(2).values(:,1);
lon_p=variables(3).values(:,1);

% Estructura de datos var(time,pert,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

%Vamos a calcular la dispersión sobre el dominio total.

for t=1:tiempos
for i=1:pert
    field=squeeze(var(t,i,:,:))';
    fwrite(bin,field,'float32',0,'ieee-le');
    %field(1,1)

    
end
end

    fclose(bin)
filein='/home/juan/reforecast/vwnd850_members_24_24_ind.nc';

wrfin=netcdf(filein,'r');

bin = fopen('/home/juan/reforecast/vwnd850_members_24_24_ind.bin','w');

%Cargamos la variable y la lat-lon seleccionada y la mascara

variables(1).values=wrfin{plot_var}(:);
variables(1).description=wrfin{plot_var}.long_name(:);
variables(1).units=wrfin{plot_var}.units(:);
variables(1).offset=wrfin{plot_var}.add_offset(:);
variables(1).factor=wrfin{plot_var}.scale_factor(:);

variables(2).values=wrfin{lat_var}(:);
variables(2).description=wrfin{lat_var}.long_name(:);
variables(2).units=wrfin{lat_var}.units(:);

variables(3).values=wrfin{lon_var}(:);
variables(3).description=wrfin{lon_var}.long_name(:);
variables(3).units=wrfin{lon_var}.units(:);

variables(4).values=wrfin{ensemble_var}(:);  %Nos dice a que miembro corresponde cada campo. -1 n1, 1 p1 y asi.


% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

%Vamos a calcular la dispersión sobre el dominio total.

for t=1:tiempos
for i=1:pert
    field=squeeze(var(t,i,:,:))';
    fwrite(bin,field,'float32',0,'ieee-le');
    
end
end
    

filein='/home/juan/reforecast/vwnd850_members_36_36_ind.nc';

wrfin=netcdf(filein,'r');

bin = fopen('/home/juan/reforecast/vwnd850_members_36_36_ind.bin','w');

%Cargamos la variable y la lat-lon seleccionada y la mascara

variables(1).values=wrfin{plot_var}(:);
variables(1).description=wrfin{plot_var}.long_name(:);
variables(1).units=wrfin{plot_var}.units(:);
variables(1).offset=wrfin{plot_var}.add_offset(:);
variables(1).factor=wrfin{plot_var}.scale_factor(:);

variables(2).values=wrfin{lat_var}(:);
variables(2).description=wrfin{lat_var}.long_name(:);
variables(2).units=wrfin{lat_var}.units(:);
%variables(2).offset=wrfin{lat_var}.add_offset(:);
%variables(2).factor=wrfin{lat_var}.scale_factor(:);

variables(3).values=wrfin{lon_var}(:);
variables(3).description=wrfin{lon_var}.long_name(:);
variables(3).units=wrfin{lon_var}.units(:);
%variables(3).offset=wrfin{lon_var}.add_offset(:);
%variables(3).factor=wrfin{lon_var}.scale_factor(:);

variables(4).values=wrfin{ensemble_var}(:);  %Nos dice a que miembro corresponde cada campo. -1 n1, 1 p1 y asi.


%Calculo el rango de latitudes y longitudes abarcado por el dominio.

minlat=min(min(variables(2).values));
minlon=min(min(variables(3).values));
maxlat=max(max(variables(2).values));
maxlon=max(max(variables(3).values));

%Obtengo los valores de la latitud y la longitud.

lat_p=variables(2).values(:,1);
lon_p=variables(3).values(:,1);

% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

for t=1:tiempos
for i=1:pert
    field=squeeze(var(t,i,:,:))';
    fwrite(bin,field,'float32',0,'ieee-le');
    
end
end

filein='/home/juan/reforecast/vwnd850_members_48_48_ind.nc';

wrfin=netcdf(filein,'r');

bin = fopen('/home/juan/reforecast/vwnd850_members_48_48_ind.bin','w');


%Cargamos la variable y la lat-lon seleccionada y la mascara

variables(1).values=wrfin{plot_var}(:);
variables(1).description=wrfin{plot_var}.long_name(:);
variables(1).units=wrfin{plot_var}.units(:);
variables(1).offset=wrfin{plot_var}.add_offset(:);
variables(1).factor=wrfin{plot_var}.scale_factor(:);

variables(2).values=wrfin{lat_var}(:);
variables(2).description=wrfin{lat_var}.long_name(:);
variables(2).units=wrfin{lat_var}.units(:);
%variables(2).offset=wrfin{lat_var}.add_offset(:);
%variables(2).factor=wrfin{lat_var}.scale_factor(:);

variables(3).values=wrfin{lon_var}(:);
variables(3).description=wrfin{lon_var}.long_name(:);
variables(3).units=wrfin{lon_var}.units(:);
%variables(3).offset=wrfin{lon_var}.add_offset(:);
%variables(3).factor=wrfin{lon_var}.scale_factor(:);

variables(4).values=wrfin{ensemble_var}(:);  %Nos dice a que miembro corresponde cada campo. -1 n1, 1 p1 y asi.


%Calculo el rango de latitudes y longitudes abarcado por el dominio.

minlat=min(min(variables(2).values));
minlon=min(min(variables(3).values));
maxlat=max(max(variables(2).values));
maxlon=max(max(variables(3).values));

%Obtengo los valores de la latitud y la longitud.

lat_p=variables(2).values(:,1);
lon_p=variables(3).values(:,1);

% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

for t=1:tiempos
for i=1:pert
    field=squeeze(var(t,i,:,:))';
    fwrite(bin,field,'float32',0,'ieee-le');
    
end
end




