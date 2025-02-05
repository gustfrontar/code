%Programa par leer los archivos de superficie del WRF.
%El dibujo solo funciona bien con proyeccion mercator (para otras
%proyecciones deforma la reticula original)


clear all;
close all;

%Definimos con que variable vamos a trabajar y las latitudes y longitudes de la reticular correspondiente.

plot_var=char('u07');
lat_var=char('lac');
lon_var=char('loc');
mask_var=char('lnd');


filein='/home/juan/matlab/scripts/static.wrfsi.d02';
change_to_nan=1.0000e+037 %Valor undef de los archivos netcdf del WRF.
run /home/juan/matlab/scripts/lista_de_variables

wrfin=netcdf(filein,'r');

%Encontramos la variable para plotear buscandola en la lista.

%x=double(char(variables(:).name));

%xi=double(plot_var);
%ivar=find(x(:,1)==xi(1)& x(:,2)==xi(2) & x(:,3)==xi(3) );

%xi=double(lat_var);
%ilat=find(x(:,1)==xi(1)& x(:,2)==xi(2) & x(:,3)==xi(3) );

%xi=double(lon_var);
%ilon=find(x(:,1)==xi(1)& x(:,2)==xi(2) & x(:,3)==xi(3) );

%xi=double(mask_var);
%imask=find(x(:,1)==xi(1)& x(:,2)==xi(2) & x(:,3)==xi(3) );

%Cargamos la variable y la lat-lon seleccionada y la mascara

variables(1).values=wrfin{plot_var}(:);
variables(1).description=wrfin{plot_var}.long_name(:);
variables(1).units=wrfin{plot_var}.units(:);

variables(2).values=wrfin{lat_var}(:);
variables(2).description=wrfin{lat_var}.long_name(:);
variables(2).units=wrfin{lat_var}.units(:);

variables(3).values=wrfin{lon_var}(:);
variables(3).description=wrfin{lon_var}.long_name(:);
variables(3).units=wrfin{lon_var}.units(:);

variables(4).values=wrfin{mask_var}(:);
variables(4).description=wrfin{mask_var}.long_name(:);
variables(4).units=wrfin{mask_var}.units(:);

%Calculo el rango de latitudes y longitudes abarcado por el dominio.

minlat=min(min(variables(2).values));
minlon=min(min(variables(3).values));
maxlat=max(max(variables(2).values));
maxlon=max(max(variables(3).values));

%Obtengo los valores de la latitud y la longitud.

lat_p=variables(2).values(:,1);
lon_p=variables(3).values(1,:)';

%Vamos a remover los undef

%j=find(variables(1).values>10000); %Esto necesita pulirse, por algun motivo no toma el valor de change_to_nan
%variables(1).values(j)=NaN

%variables(1).values(find(variables(4).values==0))=NaN;

set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
pcolor(lon_p,lat_p,variables(1).values);

run colorbar;
