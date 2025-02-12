function [lat_wrf lon_wrf hgt_wrf mask_wrf]=read_wrf_fun(file,nx_w,ny_w,endian)

%FUNCION PARA LEER EL DOMINIO DEL WRF
%LANDMASK       1  0  LAND MASK (1 FOR LAND, 0 FOR WATER) (-)
%HGT            1  0  Terrain Height (m)
%XLAT           1  0  LATITUDE, SOUTH IS NEGATIVE (degree_north)
%XLONG          1  0  LONGITUDE, WEST IS NEGATIVE (degree_east)

tot_vars=4;   %Numero total de variables (campos)

narch=fopen(file,'r',endian);
var=NaN(ny_w,nx_w,tot_vars);

if(narch  ~= -1)

 for j=1:tot_vars
            var(:,:,j)=fread(narch,[nx_w ny_w],'single')';
 end

fclose(narch);

end

%Acomodamos lo que leimos a las diferentes variables segun lo que dice el
%CTL.

mask_wrf=var(:,:,1);
hgt_wrf=var(:,:,2);
lat_wrf=var(:,:,3);
lon_wrf=var(:,:,4);


