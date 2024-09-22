% Funcion que lee los archivos de altura de las salidas del modelo WRF
% nx n de puntos en x, ny n de puntos en y
% nlevs n de niveles de las variables de altura
% arch nombre del archivo con ruta y todo.

function [ENSEMBLE]=read_ensemble(ENS_PATH,NMEMBERS,DATE,RANGE)

%Las dimesiones de la reticula del SPEEDY
NX=96;
NY=48;
NZ=37;


ENSEMBLE=NaN(NY,NX,NZ,NMEMBERS);


for iens=1:NMEMBERS
ens_str=num2str(iens+1000);
ens_str=ens_str(2:4);
    
input_file=strcat(ENS_PATH,'/',ens_str,'/',DATE,RANGE,'_p.grd')
%input_file=strcat(ENS_PATH,'/',DATE,'_p.grd')
input_file_n=fopen(input_file,'r','b');

   if(input_file_n ~= -1)
     for ivars=1:NZ
     ENSEMBLE(:,:,ivars,iens)=fread(input_file_n,[NX NY],'single')';
     end
     fclose(input_file_n);
    
    end



end






