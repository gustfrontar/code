clear all
close all

% Leo los pronoticos de ese dia, son los pronosticos de la corrida de las
% 12Z
load pp_est.mat

datos=load('-ascii','est_cuenca.txt');
tam=size(datos,1);


S=load('-ascii', 'fecha.txt');
fech=num2str(S);
fech=fech(1:8);
b=datenum(fech,'yyyymmdd');
c=b-5;
d=datevec(c);
tiempo=datestr(d, 'yyyymmdd'); %Fecha para el CMORPH y fecha para el pronostico WRF
year=tiempo(1:4);
month=tiempo(5:6);
day=tiempo(7:8);
dia=strcat(year, month, day); % Genera la fecha como un vector
dia=str2num(dia);
t=str2num(tiempo);

hoy=find(pp_acum_est(:,1)==t);
precipitacion=pp_acum_est(hoy,2:9);



for i=1:tam
    lat_est=datos(i,2);
    lon_est=datos(i,3);
    gts=find(precipitacion(:,1)==lat_est & precipitacion(:,2)==lon_est);
    tam_gts=size(gts,1);
    if isempty(gts)==0
        if (tam_gts~=1)
            gts=gts(1,:);
        end
        pp_estacion(i,:)=precipitacion(gts,:);
    end
end


promediot_24=nanmean(pp_estacion(:,4));
promediot_48=nanmean(pp_estacion(:,6));
promediot_72=nanmean(pp_estacion(:,8));

promedio_obs=nanmean(pp_estacion(:,3));


%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_promediogts_cuenca.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_promediogts_cuenca.mat
a=(size(promedio_e,1))+1;
promedio_e(a,1)=dia;
promedio_e(a,2)=promedio_obs;
promedio_e(a,3)=promediot_24;
promedio_e(a,4)=promediot_48;
promedio_e(a,5)=promediot_72;
save pp_promediogts_cuenca.mat promedio_e
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
promedio_e(1,1)=dia;
promedio_e(1,2)=promedio_obs;
promedio_e(1,3)=promediot_24;
promedio_e(1,4)=promediot_48;
promedio_e(1,5)=promediot_72;
save pp_promediogts_cuenca.mat promedio_e
end



