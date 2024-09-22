function fast_dump_record(gribfile,outpath)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib2';   
tmpfile='tmpgribfile.grib';
warning off
%==========================================================================
% ESTA FUNCION TOMA UN ARCHIVO DEL TIGGE Y LEE TODOS LOS REGISTROS Y LOS VA
% ESCRIBIENDO UNO POR UNO EN EL DISCO. EN ARCHIVOS SEPARADOS CON LA FECHA
% DE VERIFICACION, EL PLAZO DE PRONOSTICO Y EL MIEMBRO DEL ENSAMBLE EN EL
% NOMBRE.
%==========================================================================

%==========================================================================
% DECODE DATA TO A TEMPORARY FILE
%==========================================================================
cont=true;
record=1;
while(cont)
[error,record_info]=unix([wgrib ' -d ' num2str(record) ...
           ' -grib '  tmpfile ' ' gribfile ]);
       

if(error==0)
%Get the information about the record to construct the file name.
%3299:108659206:d=2008020212:HGT:500 mb:18 hour fcst:ENS=+50
record_info=strread(record_info,'%s','delimiter',':');
record_date=record_info{3};
record_date=record_date(3:end);

record_var=record_info{4};

record_level=record_info{5};
record_level=strread(record_level,'%s','delimiter',' ');
level=str2num(record_level{1});

tmpread=record_info{6};
leadtime_info=strread(tmpread,'%s','delimiter',' ');
leadtime=leadtime_info{1};
if(strcmp(leadtime,'anl'))
    nleadtime=0;
else
    nleadtime=str2num(leadtime);
end

ens_info=record_info{7};
ens_info=strread(ens_info,'%s','delimiter','=');
ensmember=str2num(ens_info{2});

if(level==500)
mkdir([outpath '/' record_date '/']);
outfile=[outpath '/' record_date '/HGT_500' record_date '_F' num2str(nleadtime) '_M' num2str(ensmember) '.grib' ]

unix(['mv ' tmpfile ' ' outfile]);
end

else
    cont=false;
end

record=record+1;
end



end
