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
dia=str2num(tiempo)


% Leo los pronoticos de ese dia, son los pronosticos de la corrida de las
% 12Z
load pp_wrf.mat
t_wrf=find(fecha_archivo(1,:)==anio & fecha_archivo(2,:)==mes & fecha_archivo(3,:)==diaa);
saber=isempty(t_wrf);



if (saber~=1)
prono_24=pp_acum_wrf(:,:,1,t_wrf);
prono_48=pp_acum_wrf(:,:,3,t_wrf);
prono_72=pp_acum_wrf(:,:,5,t_wrf);


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

% Calculo el promedio para el pronostico a 48 horas
promedio1_48=nanmean(prono_48(mascara1));
promedio2_48=nanmean(prono_48(mascara2));
promedio3_48=nanmean(prono_48(mascara3));
promedio4_48=nanmean(prono_48(mascara4));
promediot_48=nanmean(prono_48(mascarat));

% Calculo el promedio para el pronostico a 72 horas
promedio1_72=nanmean(prono_72(mascara1));
promedio2_72=nanmean(prono_72(mascara2));
promedio3_72=nanmean(prono_72(mascara3));
promedio4_72=nanmean(prono_72(mascara4));
promediot_72=nanmean(prono_72(mascarat));

else
    promedio1_24=NaN(1,1);
    promedio2_24=NaN(1,1);
    promedio3_24=NaN(1,1);
    promedio4_24=NaN(1,1);
    promediot_24=NaN(1,1);
    promedio1_48=NaN(1,1);
    promedio2_48=NaN(1,1);
    promedio3_48=NaN(1,1);
    promedio4_48=NaN(1,1);
    promediot_48=NaN(1,1);
    promedio1_72=NaN(1,1);
    promedio2_72=NaN(1,1);
    promedio3_72=NaN(1,1);
    promedio4_72=NaN(1,1);
    promediot_72=NaN(1,1);
end





%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_promedio_cuenca.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_promedio_cuenca.mat
a=(size(promedio,1))+1;
promedio(a,1)=dia;
promedio(a,2)=promedio1_24;
promedio(a,3)=promedio2_24;
promedio(a,4)=promedio3_24;
promedio(a,5)=promedio4_24;
promedio(a,6)=promediot_24;
promedio(a,7)=promedio1_48;
promedio(a,8)=promedio2_48;
promedio(a,9)=promedio3_48;
promedio(a,10)=promedio4_48;
promedio(a,11)=promediot_48;
promedio(a,12)=promedio1_72;
promedio(a,13)=promedio2_72;
promedio(a,14)=promedio3_72;
promedio(a,15)=promedio4_72;
promedio(a,16)=promediot_72;
save pp_promedio_cuenca.mat promedio
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
promedio(1,1)=dia;
promedio(1,2)=promedio1_24;
promedio(1,3)=promedio2_24;
promedio(1,4)=promedio3_24;
promedio(1,5)=promedio4_24;
promedio(1,6)=promediot_24;
promedio(1,7)=promedio1_48;
promedio(1,8)=promedio2_48;
promedio(1,9)=promedio3_48;
promedio(1,10)=promedio4_48;
promedio(1,11)=promediot_48;
promedio(1,12)=promedio1_72;
promedio(1,13)=promedio2_72;
promedio(1,14)=promedio3_72;
promedio(1,15)=promedio4_72;
promedio(1,16)=promediot_72;
save pp_promedio_cuenca.mat promedio
end


