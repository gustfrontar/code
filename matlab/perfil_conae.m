%SCRIPT PARA LEER LAS SALIDAS DEL WRF OPERATIVO Y OBTENER EL PERFIL DE
%VIENTOS EN LA VERTICAL DEL PUNTO QUE QUIERE LA CONAE

%Definimos el nombre del archivo
%filenamein='salidaposta';

%Definimos la fecha de inicializacion
%fecha='2710201100';

%Definimos nuestro punto de inter�s en la grilla del WRF
row_point=110;
column_point=169;

row_point_u=110;
column_point_u=170;

row_point_v=111;
column_point_v=169;

%Obtenemos las variables que queremos

%Variable latitud
% out=get_var_nc(filenamein,'XLAT');
% 
% XLAT=squeeze(out.data(:,:,1));
% 
% clear out
% 
% %Definimos el tama�o de la grilla en la horizontal
% [nlat nlon]=size(XLAT);
% 
% %Variable longitud
% out=get_var_nc(filenamein,'XLONG');
% 
% XLONG=squeeze(out.data(:,:,1));
% 
% clear out
% 
% %Variable perturbacion de geopotencial
% out=get_var_nc(filenamein,'XLAT_U');
% 
% XLAT_U=squeeze(out.data(:,:,1));
% 
% clear out
% 
% %Variable geopotencial del estado basico
% out=get_var_nc(filenamein,'XLONG_U');
% 
% XLONG_U=squeeze(out.data(:,:,1));
% 
% clear out
% 
% %Variable perturbacion de geopotencial
% out=get_var_nc(filenamein,'XLAT_V');
% 
% XLAT_V=squeeze(out.data(:,:,1));
% 
% clear out
% 
% %Variable geopotencial del estado basico
% out=get_var_nc(filenamein,'XLONG_V');
% 
% XLONG_V=squeeze(out.data(:,:,1));
% 
% clear out

%Variable niveles eta de las variables U,V
out=get_var_nc(filenamein,'ZNU');

ZNU=squeeze(out.data(:,1));

clear out

%Variable niveles eta de las variables de masa y W
out=get_var_nc(filenamein,'ZNW');

ZNW=squeeze(out.data(:,1));

clear out


%Variable viento zonal
out=get_var_nc(filenamein,'U');

U=squeeze(squeeze(out.data(row_point_u,column_point_u,:,:)));

clear out

%Variable viento meridional
out=get_var_nc(filenamein,'V');

V=squeeze(squeeze(out.data(row_point_v,column_point_v,:,:)));

clear out

%Variable velocidad vertical
out=get_var_nc(filenamein,'W');

W=squeeze(squeeze(out.data(row_point,column_point,:,:)));

clear out

%Variable perturbacion de geopotencial
out=get_var_nc(filenamein,'PH');

PH=squeeze(squeeze(out.data(row_point,column_point,:,:)));

clear out

%Variable geopotencial del estado basico
out=get_var_nc(filenamein,'PHB');

PHB=squeeze(squeeze(out.data(row_point,column_point,:,:)));

clear out

%Nos armamos la altura real de la atmosfera aproximada
altura=(PH+PHB)/9.81;

clear PH PHB

%Variable seno de la rotacion del mapa
out=get_var_nc(filenamein,'SINALPHA');

SINALPHA=squeeze(squeeze(out.data(row_point,column_point,1)));

clear out

%Variable coseno de la rotacion del mapa
out=get_var_nc(filenamein,'COSALPHA');

COSALPHA=squeeze(squeeze(out.data(row_point,column_point,1)));

clear out

%Variable altura de la capa limite
out=get_var_nc(filenamein,'PBLH');

PBLH=squeeze(squeeze(out.data(row_point,column_point,:)));

clear out

%Corregimos los vientos por la rotacion del mapa
Urot(:,:)=V(:,:)*SINALPHA+U(:,:)*COSALPHA;
Vrot(:,:)=V(:,:)*COSALPHA-U(:,:)*SINALPHA;


%Interpolo en la vertical los vientos U y V para tener todas las variables
%en los mismos niveles
for i=1:49
    
    Uint(:,i)=interp1(ZNU,Urot(:,i),ZNW(2:49));
    Vint(:,i)=interp1(ZNU,Vrot(:,i),ZNW(2:49));

end

%Escribimos los archivos de salida en formato txt

for i=2:49

   hour=num2str(i-1);
  
  if i < 11
    
   hour=strcat('0',hour);
      
  end

  name_file=strcat(fecha,'_',hour,'.txt');
  fid=fopen(name_file,'w');
  
  for j=2:49
  
      output(j-1,:)=[altura(j,i) fix(Uint(j-1,i)*10) fix(Vint(j-1,i)*10) fix(W(j,i)*10)];
      fprintf(fid,'%5.0f  %4.0f  %4.0f  %4.0f\n',output(j-1,:));
     
  end
  
  fclose(fid);
    
end    
