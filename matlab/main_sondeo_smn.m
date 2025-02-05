
clear all
close all

server='ftp.smn.gov.ar';
user='cima';
pass='smncima13';

horas=[00 12];

%==========================================================================
% EZEIZA
%==========================================================================

for ii=1:length(horas)

horasondeo=horas(ii);

today=date;
tmp=datenum(today);
today=datestr(tmp,'yyyymmdd');

filename=['TEMP' today '_' num2str(horasondeo) ];
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


end


