% Funcion que lee los archivos de altura de las salidas del modelo WRF
% nx n de puntos en x, ny n de puntos en y
% nlevs n de niveles de las variables de altura
% arch nombre del archivo con ruta y todo.

function [OUTPUT]=read_speedy(ENS_PATH,DATE)

%Las dimesiones de la reticula del SPEEDY
NX=96;
NY=48;
NZ=37;


OUTPUT=NaN(NY,NX,NZ);

input_file=strcat(ENS_PATH,'/',DATE,'_p.grd');
input_file_n=fopen(input_file,'r','b');

   if(input_file_n ~= -1)
     for ivars=1:NZ
     OUTPUT(:,:,ivars)=fread(input_file_n,[NX NY],'single')';
     end
     fclose(input_file_n);
    
    end









