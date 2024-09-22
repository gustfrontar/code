function fast_dump_record_cfsr(gribfile,outpath,tmpfile)
% DUMP RECORDS AND INTERPOLATE DATA FROM CFSR TO TIGGE GRID.
wgrib='wgrib2';   
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
           ' -bin '  tmpfile ' ' gribfile ]);
       

if(error==0)
%Get the information about the record to construct the file name.
%3299:108659206:d=2008020212:HGT:500 mb:anl 
record_info=strread(record_info,'%s','delimiter',':');
record_date=record_info{3};
record_date=record_date(3:end);

record_var=record_info{4};

record_level=record_info{5};
record_level=strread(record_level,'%s','delimiter',' ');
level=str2num(record_level{1});

if(level==500)
    
outfile=[outpath '/CFSR_HGT_' record_date '.bin' ];

unix(['mv ' tmpfile ' ' outfile]);
end

else
    cont=false;
end

record=record+1;
end



end
