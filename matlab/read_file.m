% Funcion que lee los archivos de altura de las salidas del modelo WRF
% nx n de puntos en x, ny n de puntos en y
% nlevs n de niveles de las variables de altura
% arch nombre del archivo con ruta y todo.

function var=read_var(nx,ny,nlevs,arch)


narch=fopen(arch,'r','l'); %Open para leer (r) y en formato little_endian (l).

if(narch  ~= -1) %si es -1 quiere decir que no encontro el archivo.
for i=1:nlevs
nada=fread(narch,[2 1],'single')';  %En archivos sequenciales es necesario agregar este record antes de la lectura del campo
                                    %Si el archivo es acceso directo esto no va.
var(:,:,i)=fread(narch,[nx ny],'single')'; %lee del archivo una matriz de tamaño nx ny con precisión simple.
end
for i=1:nlevs
nada=fread(narch,[2 1],'single')';
var(:,:,i+nlevs)=fread(narch,[nx ny],'single')';
end
for i=1:nlevs
nada=fread(narch,[2 1],'single')';
var(:,:,i+2*nlevs)=fread(narch,[nx ny],'single')';
end
for i=1:nlevs
nada=fread(narch,[2 1],'single')';
var(:,:,i+3*nlevs)=fread(narch,[nx ny],'single')';
end
for i=1:nlevs
nada=fread(narch,[2 1],'single')';
var(:,:,i+4*nlevs)=fread(narch,[nx ny],'single')';
end
for i=1:nlevs
nada=fread(narch,[2 1],'single')';
var(:,:,i+5*nlevs)=fread(narch,[nx ny],'single')';
end
nada=fread(narch,[2 1],'single')';
var(:,:,1+6*nlevs)=fread(narch,[nx ny],'single')';

fclose(narch);
else
for i=1:nlevs*6+1
    var(:,:,i)=NaN(ny,nx);
end
end

var(find(var>=1000000))=NaN;