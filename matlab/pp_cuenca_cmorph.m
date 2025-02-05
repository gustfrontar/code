clear all
close all

load mask_interpolada.mat


S=load('-ascii', 'fecha.txt');
fech=num2str(S);
fech=fech(1:8);
b=datenum(fech,'yyyymmdd');
c=b-5;
d=datevec(c);
tiempo=datestr(d, 'yyyymmdd'); %Fecha para el CMORPH y fecha para el pronostico WRF
anio=str2num(tiempo(1:4));
mes=str2num(tiempo(5:6));
diaa=str2num(tiempo(7:8));
dia=str2num(tiempo);


% Leo los pronoticos de ese dia, son los pronosticos de la corrida de las
% 12Z
load pp_cmorph.mat
t_cmorph=find(fecha_archivo(1,:)==anio & fecha_archivo(2,:)==mes & fecha_archivo(3,:)==diaa);
saber=isempty(t_cmorph);




if (saber~=1)
 prono_24=pp_acum_cmorph(:,:,t_cmorph);
 
% Transformo en logical la variable de la mascar y calculo el promedio
% sobre la mascara 1 para el pronostico a 24 horas
mascara1=logical(mascara1);
promedio1_24=nanmean(prono_24(mascara1));

mascara2=logical(mascara2);
promedio2_24=nanmean(prono_24(mascara2));

mascara3=logical(mascara3);
promedio3_24=nanmean(prono_24(mascara3));

mascara4=logical(mascara4);
promedio4_24=nanmean(prono_24(mascara4));

mascarat=logical(mascarat);
promediot_24=nanmean(prono_24(mascarat));

else
    promedio1_24=NaN(1,1);
    promedio2_24=NaN(1,1);
    promedio3_24=NaN(1,1);
    promedio4_24=NaN(1,1);
    promediot_24=NaN(1,1);
end





%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_promediocmorph_cuenca.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_promediocmorph_cuenca.mat
a=(size(promedio_c,1))+1;
promedio_c(a,1)=dia;
promedio_c(a,2)=promedio1_24;
promedio_c(a,3)=promedio2_24;
promedio_c(a,4)=promedio3_24;
promedio_c(a,5)=promedio4_24;
promedio_c(a,6)=promediot_24;
save pp_promediocmorph_cuenca.mat promedio_c
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
promedio_c(1,1)=dia;
promedio_c(1,2)=promedio1_24;
promedio_c(1,3)=promedio2_24;
promedio_c(1,4)=promedio3_24;
promedio_c(1,5)=promedio4_24;
promedio_c(1,6)=promediot_24;
save pp_promediocmorph_cuenca.mat promedio_c
end

