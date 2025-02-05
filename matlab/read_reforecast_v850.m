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

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

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
    
        ensdisp=(ensdisp/pert).^0.5;
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
        
        ensdisp12(in,jj)=0;
        ensdisp12(is,jj)=0;

set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
subplot(2,2,1);
pcolor(lon_p,lat_p,ensdisp12);

run colorbar;


filein='/home/juan/reforecast/vwnd850_members_24_24_ind.nc';

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

% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

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
        ensdisp24=squeeze(mean(ensdisp,1));
        edisp(2)=squeeze(mean(mean(ensdisp24)));
       
        in=find(variables(2).values>=latminn & variables(2).values<=latmaxn);
        is=find(variables(2).values>=latmins & variables(2).values<=latmaxs);
        it=find(variables(2).values>=latmins & variables(2).values<=latmaxn);
        jj=find(variables(3).values>=lonmins & variables(3).values<=lonmaxs);
        ensdispn=ensdisp24(in,jj);
        ensdisps=ensdisp24(is,jj);
        ensdispt=ensdisp24(it,jj);
        

        
        edispn(2)=squeeze(mean(mean(ensdispn)));
        edisps(2)=squeeze(mean(mean(ensdisps)));
        edispt(2)=squeeze(mean(mean(ensdispt)));

set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
subplot(2,2,2);
pcolor(lon_p,lat_p,ensdisp24);


run colorbar;


filein='/home/juan/reforecast/vwnd850_members_36_36_ind.nc';

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

% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

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
        ensdisp36=squeeze(mean(ensdisp,1));
        edisp(3)=squeeze(mean(mean(ensdisp36)));
       
        in=find(variables(2).values>=latminn & variables(2).values<=latmaxn);
        is=find(variables(2).values>=latmins & variables(2).values<=latmaxs);
        it=find(variables(2).values>=latmins & variables(2).values<=latmaxn);
        jj=find(variables(3).values>=lonmins & variables(3).values<=lonmaxs);
        ensdispn=ensdisp36(in,jj);
        ensdisps=ensdisp36(is,jj);
        ensdispt=ensdisp36(it,jj);
        

        
        edispn(3)=squeeze(mean(mean(ensdispn)));
        edisps(3)=squeeze(mean(mean(ensdisps)));
        edispt(3)=squeeze(mean(mean(ensdispt)));




set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
subplot(2,2,3);
pcolor(lon_p,lat_p,ensdisp36);

run colorbar;


filein='/home/juan/reforecast/vwnd850_members_48_48_ind.nc';

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

% Estructura de datos var(lat,lon,pert,xx,lon,lat)

var=squeeze(variables(1).values(:,:,1,:,:).*variables(1).factor+variables(1).offset);

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
        ensdisp48=squeeze(mean(ensdisp,1));
        edisp(4)=squeeze(mean(mean(ensdisp48)));
        
       
        in=find(variables(2).values>=latminn & variables(2).values<=latmaxn);
        is=find(variables(2).values>=latmins & variables(2).values<=latmaxs);
        it=find(variables(2).values>=latmins & variables(2).values<=latmaxn);
        jj=find(variables(3).values>=lonmins & variables(3).values<=lonmaxs);
        ensdispn=ensdisp48(in,jj);
        ensdisps=ensdisp48(is,jj);
        ensdispt=ensdisp48(it,jj);
        

        
        edispn(4)=squeeze(mean(mean(ensdispn)));
        edisps(4)=squeeze(mean(mean(ensdisps)));
        edispt(4)=squeeze(mean(mean(ensdispt)));



set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
subplot(2,2,4);
pcolor(lon_p,lat_p,ensdisp48);

run colorbar;

figure
plot(edispt)
hold on
plot(edisps,'r')
hold on
tit=title('Azul (T), Verde (N), Rojo (S)')
get(tit)
plot(edispn,'g')



