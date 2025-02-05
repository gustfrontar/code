clear all
close all
%*************************************************************************
%Este programa crea el conjunto de datos de precipitacon del Super Ensemble
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%Vamos a usar la funcion precip_model_fun para generar el conjunto.

path='/WRFV2/wrfsi/domains/operativo/matlab/';

arc=strcat(path,'fech.txt')
S=load('-ascii', arc);
tiempo=num2str(S);    %Pasamos la fecha que leimos a un string.
ini_date=tiempo;
end_date=tiempo;
tiem=ones(344,1)*S; % Me genero un vector con la fecha del dia q se abren los archivos

[p24eta40 p48eta40 est lat lon]=precip_model_fun('ETAxx','12',5,5,ini_date,end_date);
[p24eta40_0 p48eta40_0 est lat lon]=precip_model_fun('ETAxx','00',5,5,ini_date,end_date);
[p24eta20 p48eta20 est lat lon]=precip_model_fun('ETA20','12',5,5,ini_date,end_date);
[p24eta20_0 p48eta20_0 est lat lon]=precip_model_fun('ETA20','00',5,5,ini_date,end_date);
[p24t213x p48t213x est lat lon]=precip_model_fun('T213x','12',4,5,ini_date,end_date);
[p24t213x_0 p48t213x_0 est lat lon]=precip_model_fun('T213x','00',5,5,ini_date,end_date);
[p24b_uba p48b_uba est lat lon]=precip_model_fun('B_UBA','12',9,9,ini_date,end_date);
[p24b_uba_0 p48b_uba_0 est lat lon]=precip_model_fun('B_UBA','00',9,9,ini_date,end_date);
[p24hrmxx p48hrmxx est lat lon]=precip_model_fun('HRMxx','12',9,9,ini_date,end_date);
[p24hrmxx2 p48hrmxx2 est lat lon]=precip_model_fun('HRMxx','00',9,9,ini_date,end_date);
[p24rpsas p48rpsas est lat lon]=precip_model_fun('RPSAS','12',5,5,ini_date,end_date);
[p24rpsas_0 p48rpsas_0 est lat lon]=precip_model_fun('RPSAS','00',5,5,ini_date,end_date);
[p24catt p48catt est lat lon]=precip_model_fun('CATTx','00',9,9,ini_date,end_date);
[p24wrfar_0 p48wrfar_0 est lat lon]=precip_model_fun('WRFAR','00',9,9,ini_date,end_date);
[p24wrfar p48wrfar est lat lon]=precip_model_fun('WRFAR','12',9,9,ini_date,end_date);
[p24etaum p48etaum est lat lon]=precip_model_fun('ETAUM','00',5,5,ini_date,end_date);
[p24mrf p48mrf est lat lon]=precip_model_fun('MRFxx','00',3,3,ini_date,end_date);
[p24sfenm p48sfenm est lat lon]=precip_model_fun('SFENM','12',3,3,ini_date,end_date);
[p24sfenm_0 p48sfenm_0 est lat lon]=precip_model_fun('SFENM','00',3,3,ini_date,end_date);

% Me genero un vector de Nan q luego sera completado con los datos de
% observacion y los datos Cmorph
obs=ones(344,1)*NaN;
cmor=ones(344,1)*NaN;
trmm=ones(344,1)*NaN;

b=size(est,1);
p24(:,1)=tiem ;
p24(:,2)=est;
p24(:,3)=lat;
p24(:,4)=lon;
p24(:,5)=obs;
p24(:,6)=cmor;
p24(:,7)=trmm;
p24(:,8)=p24eta40;
p24(:,9)=p24eta40_0;
p24(:,10)=p24eta20;
p24(:,11)=p24eta20_0;
p24(:,12)=p24t213x;
p24(:,13)=p24t213x_0;
p24(:,14)=p24b_uba;
p24(:,15)=p24b_uba_0;
p24(:,16)=p24hrmxx;
p24(:,17)=p24hrmxx2;
p24(:,18)=p24rpsas;
p24(:,19)=p24rpsas_0;
p24(:,20)=p24catt;
p24(:,21)=p24wrfar_0;
p24(:,22)=p24wrfar;
p24(:,23)=p24etaum;
p24(:,24)=p24mrf;
p24(:,25)=NaN;
p24(:,26)=NaN;

p48(:,1)=tiem ;
p48(:,2)=est;
p48(:,3)=lat;
p48(:,4)=lon;
p48(:,5)=obs;
p48(:,6)=cmor;
p48(:,7)=trmm;
p48(:,8)=p48eta40;
p48(:,9)=p48eta40_0;
p48(:,10)=p48eta20;
p48(:,11)=p48eta20_0;
p48(:,12)=p48t213x;
p48(:,13)=p48t213x_0;
p48(:,14)=p48b_uba;
p48(:,15)=p48b_uba_0;
p48(:,16)=p48hrmxx;
p48(:,17)=p48hrmxx2;
p48(:,18)=p48rpsas;
p48(:,19)=p48rpsas_0;
p48(:,20)=p48catt;
p48(:,21)=p48wrfar_0;
p48(:,22)=p48wrfar;
p48(:,23)=p48etaum;
p48(:,24)=p48mrf;
p48(:,25)=NaN;
p48(:,26)=NaN;


%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_superensemble.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato 
load pp_superensemble.mat
pp_ensemble_24=[pp_ensemble_24 ; p24];
pp_ensemble_48=[pp_ensemble_48 ; p48];
save pp_superensemble.mat pp_ensemble_24 pp_ensemble_48
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
pp_ensemble_24=p24;
pp_ensemble_48=p48;
save pp_superensemble.mat pp_ensemble_24 pp_ensemble_48
end
