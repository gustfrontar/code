%Interpola las salidas del WRF global a la reticula de los reanalisis del
%NCEP.

clear all
close all

%Dimensiones de la reticula del WRF
nx_w=360;
ny_w=179;
wrf_nvars=30;

%==========================================================================
% leemos las lat y lon del WRF. %LAS LAT Y LONS SE PUEDEN LEER DEL NETCDF
% O PEDIRLE AL ARWPOST QUE LAS INCLUYA EN LOS ARCHIVOS BINARIOS.
aux=read_wrf_fun(nx_w,ny_w,2,'../global_latlon.dat','b');
lat_wrf=aux(:,:,1);
lon_wrf=aux(:,:,2);
lon_wrf(lon_wrf < 0)=360+lon_wrf(lon_wrf < 0);

% generamos las lat y lon del NCEP.
ncep_resolution=1;
lon=0:ncep_resolution:360-ncep_resolution;
lat=-90:ncep_resolution:90;
[lon_ncep lat_ncep]=meshgrid(lon,lat);
[ny_ncep nx_ncep]=size(lat_ncep);
%==========================================================================

%Calculamos que elementos de la reticula del WRF tenemos que sumar para
%generar la reticula del NCEP.
%LA IDEA DE ESTO ES QUE VAMOS A INTERPOLAR VARIOS CAMPOS EN DONDE LA POSICION
%INICIAL DE LOS PUNTOS Y LA FINAL ES LA MISMA (SOLO CAMBIAN LOS VALORES) ENTONCES
%GUARDAMOS EN UNA VARIABLE (LUT) QUE ES UNA CELDA DE MATLAB LA INFO QUE NECESITAMOS
%Y ESTO ES PARA CADA PUNTO DE LA RETICULA DEL GDAS QUE PUNTOS DE LA RETICULA DEL WRF
%TENGO QUE PROMEDIAR (ESTO ASUMIENDO QUE VAMOS A INTERPOLAR USANDO EL PROMEDIO POR CAJAS
%O BOX AVERAGING QUE BASICAMENTE PROMEDIA TODOS LOS PUNTOS DE LA RETICULA DE ALTA RESOLUCION
%QUE ESTAN DENTRO DEL AREA ASOCIADA A CADA PUNTO DE RETICULA DE BAJA RESOLUCION PARA OBTENER
%EL VALOR EN EL PUNTO DE RETICULA DE BAJA RESOLUCION (EN ESTE CASO EL GDAS).

for iy=1:ny_ncep
    for ix=1:nx_ncep
        
         %Defino los bordes de la caja asociada a cada punto 
         %de reticula de baja resolucion.
         y_s=lat_ncep(iy,ix)-(ncep_resolution)/2;
         y_n=lat_ncep(iy,ix)+(ncep_resolution)/2;

         x_w=lon_ncep(iy,ix)-(ncep_resolution)/2;
         x_e=lon_ncep(iy,ix)+(ncep_resolution)/2;
         
         %Busco los puntos dentro de la caja definida previamente.
         %*****************************************************************
         lut{iy,ix}=find(  lat_wrf < y_n & lat_wrf >= y_s & lon_wrf < x_e & lon_wrf >= x_w );
         %*****************************************************************
         %En cada punto lugar iy ix de lut lo que se guarda es una lista de puntos
         %de la reticula de alta resolucion (en realidad se guarda la lista de subindices de
         %de la matriz que corresponden a dichos puntos).
         %lut{iy,ix} es un vector de subindices de la matriz de alta resolucion que tengo que 
         %promediar para obtener el elemento iy,ix de la matriz de baja resolucion (GDAS)
    end
end


%El loop que viene a continuacion es para interpolar varios pronosticos a distintos plazos
%e inicializados en diferentes fechas. (lo dejo como esta, pero no se si esto en particular te va 
%a servir. 
ini_date_exp='2003010100';
end_date_exp='2003013100';

%Este for es porque tengo pronosticos de 0 a 15 dias con 12 horas de diferencia
%entre salida y salida.
%Las salidas estan guardadas en mi caso en un archivo por cada tiempo. 
for iforecast=0:0.5:15

ini_date=datestr(datenum(ini_date_exp,'yyyymmddHH')+iforecast,'yyyymmddHH');
end_date=datestr(datenum(end_date_exp,'yyyymmddHH')+iforecast,'yyyymmddHH');

forecast=iforecast*24;
forecast_str=num2str(forecast);
if(forecast < 10)
    forecast_str=['0' forecast_str];
end
if(forecast < 100)
    forecast_str=['0' forecast_str];
end

display(['The current forecast cycle is ' forecast_str])
EST_FREC=48;
INI_DATE_NUM=datenum(ini_date,'yyyymmddHH');
END_DATE_NUM=datenum(end_date,'yyyymmddHH');
C_DATE_NUM=INI_DATE_NUM;

while ( C_DATE_NUM <= END_DATE_NUM )
  datestr(C_DATE_NUM,'yyyymmddHH')
  ITIME=(C_DATE_NUM-INI_DATE_NUM)*24/EST_FREC+1;
  %Construye el nombre del archivo que va a leer y tambien del archivo donde va a escribir
  %los campos interpolados.
  wrf_file=['../SALLJEX/3D' datestr(C_DATE_NUM,'yyyymmddHH') 'd01F' forecast_str '.dat'];
  wrf_out_file=['../SALLJEX/3DLOWRES' datestr(C_DATE_NUM,'yyyymmddHH') 'd01F' forecast_str '.dat'];
  
  %Read field.
  aux=read_wrf_fun(nx_w,ny_w,wrf_nvars,wrf_file,'b');
  
  
  %Interpolate using the lut.
  %Aca es donde hace la interpolacion usando lo que calculo mas arriba.
  %para cada punto iy,ix de la reticula del NCEP toma la lista de puntos
  %guardados en el elemento iy,ix de la variable lut y calcula el promedio
  %de las diferentes variables sobre dichos puntos. 
  interp_aux=NaN(ny_ncep,nx_ncep,wrf_nvars);
  for ivar=1:wrf_nvars
     tmp=aux(:,:,ivar);  
    for iy=1:ny_ncep
    for ix=1:nx_ncep
            if(~isempty(lut{iy,ix}))
            interp_aux(iy,ix,ivar)=nanmean(tmp(lut{iy,ix}));
            end
    end
    end   
  end
  %Write out the results.
  %Si tiene algun NaN lo escribe como -999 porque cuando el grads
  %lee si encuentra un NaN se vuelve medio loco. 
  interp_aux(isnan(interp_aux))=-999;
  write_wrf_fun(interp_aux,wrf_out_file,'b')
  
  
C_DATE_NUM=C_DATE_NUM+2;  
end

end




