clear all
close all
%Este script desmenuza los archivos de TIGGE que son extremandamente
%grandes en archivos mas chicos. Esto agiliza mucho la busqueda dentro de
%los archivos y permite saber con seguridad cual es el tiempo y coordenadas
%de los datos que se estan carganod. 

%ESTA VERSION ES PARA LOS DATOS DEL CWAO QUE VIENEN EN UN FORMATO DIFERENTE PORQUE NO ESTA ESPECIFICADO EL NUMERO DE ENSAMBLE AL CUAL CORRESPONDE CADA CAMPO.

start_date='200710';
end_date='200712';
model='cwao';
datapath=['/home/jruiz/TIGGE/' model '/'];
outpath=['/home/jruiz/TIGGE/' model '/SMALLGRIB/'];
mkdir(outpath)
start_date_num=datenum(start_date,'yyyymm');
end_date_num=datenum(end_date,'yyyymm');

c_date_num=start_date_num;

while ( c_date_num <= end_date_num)

    TIGGE_FILE=[datapath '/HGT_ENS_' model '_' datestr(c_date_num,'yyyymm') '.grib'];
    
    fast_dump_record_cwao(TIGGE_FILE,outpath);

c_date_num=c_date_num+32;
c_date_num=datenum(datestr(c_date_num,'yyyymm'),'yyyymm');

end
%Ahora hay que acomodar este script para que barra los distintos archivos.

