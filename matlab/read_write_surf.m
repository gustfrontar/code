%Programa par leer los archivos de superficie del WRF.


%Para modificar uso de suelo.


clear all;
close all;

%Definimos con que variable vamos a trabajar y las latitudes y longitudes de la reticular correspondiente.

%plot_var es la variable que se va a modificar.
%water_use es la categoria que corresponde al agua.
plot_var=char('u19');
lat_var=char('lac');
lon_var=char('loc');
mask_var=char('lnd');
water_use=char('u16');

%Define la lista de variables contenidas en el archivo.
run /home/juan/matlab/scripts/lista_de_variables

%Define la region donde vamos a hacer cambios

%dominio cuenca
%reg_latmax=-24;
%reg_latmin=-38;
%reg_lonmax=-52;
%reg_lonmin=-64;

%dominio NOA
reg_latmax=-24;
reg_latmin=-38;
reg_lonmax=-62;
reg_lonmin=-68;

%
%Archivo de entrada.
%filein='/pronostico/WRFV2/wrfsi/domains/bnoa2/static/static.prueba';
filein='/home/juan/matlab/scripts/static.wrfsi.d02';

%Lectura con permiso de escritura
wrfin=netcdf(filein,'wr');


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

variables(5).values=wrfin{water_use}(:);
variables(5).description=wrfin{water_use}.long_name(:);
variables(5).units=wrfin{water_use}.units(:);



%Calculo el rango de latitudes y longitudes abarcado por el dominio.

minlat=min(min(variables(2).values));
minlon=min(min(variables(3).values));
maxlat=max(max(variables(2).values));
maxlon=max(max(variables(3).values));

%Obtengo los valores de la latitud y la longitud.

lat_p=variables(2).values(:,1);
lon_p=variables(3).values(1,:)';

%Vamos a generar un cambio en el uso de suelo


%i son los indices donde se verifica que lat y lon están dentro del
%cuadrado y que es tierra.
i=find(variables(2).values>=reg_latmin & variables(2).values <= reg_latmax & variables(3).values >= reg_lonmin & variables(3).values <= reg_lonmax & variables(4).values == 1)

%Definimos la categoria plot_var como 1- el valor del agua para retener el
%efecto de los rios.
variables(1).values(i)=1-variables(5).values(i);


%Ponemos en 0 las demas categorias (menos el agua que despues la
%recuperamos).
for ivar=22:44;
varaux=wrfin{variables(ivar).name}(:);
varaux(i)=0;
wrfin{variables(ivar).name}(:)=varaux(:);
end


%Escribimos plot_var y el agua sin modificar...
wrfin{plot_var}(:)=variables(1).values(:);
wrfin{water_use}(:)=variables(5).values(:);



%Vamos a remover los undef

j=find(variables(1).values>10000); %Esto necesita pulirse, por algun motivo no toma el valor de change_to_nan
variables(1).values(j)=NaN

variables(1).values(find(variables(4).values==0))=NaN;

set (gca,'XLim',[minlon maxlon]);
set (gca,'YLim',[minlat maxlat]);
pcolor(lon_p,lat_p,variables(1).values);

run colorbar;
