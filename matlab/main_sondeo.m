
clear all
close all

server='ftp.smn.gov.ar';
user='cima';
pass='smncima13';

horas={'00';'12'};

gifpath='/home/wrf/wwwuser';

%==========================================================================
% EZEIZA
%==========================================================================

for ii=1:length(horas)

horasondeo=horas{ii};

today=date;
tmp=datenum(today);
today=datestr(tmp,'yyyymmdd');

filename=['TEMP' today '_' horasondeo ];

%Miro si el archivo esta en la carpeta.
a=dir(filename);
if(isempty(a))

ftphandle = ftp(server,user,pass);
cd(ftphandle,'sondeos')
mget(ftphandle,filename)

close(ftphandle);

[TPERFIL,TDPERFIL,PPERFIL,VELPERFIL,DIRPERFIL,ID,FECHA,HORA]=read_temp_fun(filename);

if( sum(isnan(PPERFIL)) < length(PPERFIL))

sondeo_fun(PPERFIL,TPERFIL,TDPERFIL,VELPERFIL,DIRPERFIL,ID,FECHA,HORA);

else
    
 display(['Warning: No pude bajar los datos']);   

end

else  
    
  display(['Warning: El archivo ya estaba bajado, no hice nada']);
    
end   %End sobre si el archivo existia.

end

system(['mv *.png  ' gifpath]);

