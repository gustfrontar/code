%Programa par leer reforecast


clear all;
close all;

tiempos=31 %(2 meses de datos)
pert=15    %(7 pares + 1 control)

%Definimos la zona norte

latminn=-90;
latmaxn=0;
lonminn=0;
lonmaxn=360;

%Definimos la zona sur

latmins=-90;
latmaxs=0;
lonmins=0;
lonmaxs=360;

%Definimos con que variable vamos a trabajar y las latitudes y longitudes de la reticular correspondiente.

plot_var=char('hgt');
lat_var=char('lat');
lon_var=char('lon');
ensemble_var=char('ensemble_member');

filein='/home/juan/reforecast/salljex/hgt250_members_12_12_ind.nc';

wrfin=netcdf(filein,'r');


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

% Estructura de datos var(time,pert,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:)+variables(1).offset);

%Vamos a calcular la dispersión sobre el dominio total.

    
    %Calculo la media del ensamble.
    
    ensmean=squeeze(mean(var,2)); %Promediamos sobre la segunda dimension que son los miembros del ensemble.
    
    for i=1:pert
       
        dif=((squeeze(var(:,pert,:,:))-ensmean).^2);
        if ( i > 1 )
            ensdisp=ensdisp+dif;
        end
        if (i == 1 )
            ensdisp=dif;
        end
        
    end
    
        ensdisp=sqrt(ensdisp/pert);
        ensdisp12=squeeze(mean(ensdisp,1));
        edisp(1)=squeeze(mean(mean(ensdisp12)));
        
        in=find(variables(2).values>=latminn & variables(2).values<=latmaxn);
        is=find(variables(2).values>=latmins & variables(2).values<=latmaxs);
        it=find(variables(2).values>=latmins & variables(2).values<=latmaxn);
        jj=find(variables(3).values>=lonmins & variables(3).values<=lonmaxs);
        
        ensdispn=ensdisp12(in,jj);
        ensdisps=ensdisp12(is,jj);
        ensdispt=ensdisp12(it,jj);
        
        edispn(1)=squeeze(mean(mean(ensdispn)));
        edisps(1)=squeeze(mean(mean(ensdisps)));
        edispt(1)=squeeze(mean(mean(ensdispt)));
        
        %ensdisp12(in,jj)=0;
        %ensdisp12(is,jj)=0;

set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
pcolor(lon_p,lat_p,ensdisp12);

run colorbar;




