clear all
close all

%la dimension 1 es el tiempo, la 2 los parametros y la 3 los experimentos y la 4 las inicializaciones.
%input.rmse contiene la evolucion del rmse para las diferentes variables y
%los diferentes experimentos la dimension 1 es el tiempo, la 2 las
%variables y la 3 los experimentos y la 4 las inicializaciones.

%input.parnames son los nombres de los parametros que se estan estimando en
%orden. 
%input.expnames es el nombre del experimento.
%input.ctrlrmse el rmse del control la dimension 1 es el tiempo, la
%dimension 2 son las variables y la dimension 3 el expermiento de control. 
%input.truepar la evolucion del parametro para la verdad la dimension 1 es
%el tiempo, la dimension 2 es el parametro.
%input.controlname es el nombre de los diferentes experimentos de control.
%input.parmin minimun parameter values.
%input.parmax maximun parameter values.


input.expnames{1}='VHIFREC SIM';
input.expnames{2}='VHIFREC PAR';

input.controlname{1}='PERFECT MODEL';
input.controlname{2}='IMPERFECT MODEL';

input.parnames{1}='TRCNV';
input.parnames{2}='RHBL';
input.parnames{3}='RHIL';
input.parnames{4}='ENTMAX';
input.parnames{5}='SMF';

PAR_FILE='../DATA/ALLPARAMETERS_TRCNVINIT01/parameters.tbl';

input.truepar=[0.166 0.9 0.7 0.5 0.8];

%Sensibilidad del parametro TRCNV a la condicion inicial.

load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV01/parameters.mat
input.par(:,:,1,1)=mean_par1d;
input.truepar=par1dn;
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV02/parameters.mat
input.par(:,:,1,2)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV05/parameters.mat
input.par(:,:,1,3)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV1/parameters.mat
input.par(:,:,1,4)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV01/parameters.mat
input.par(:,:,2,1)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV02/parameters.mat
input.par(:,:,2,2)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV05/parameters.mat
input.par(:,:,2,3)=mean_par1d;
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV1/parameters.mat
input.par(:,:,2,4)=mean_par1d;

load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV01/analysis_rmse.mat
input.rmse(:,:,1,1)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV02/analysis_rmse.mat
input.rmse(:,:,1,2)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV05/analysis_rmse.mat
input.rmse(:,:,1,3)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFREC_TRCNV1/analysis_rmse.mat
input.rmse(:,:,1,4)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV01/analysis_rmse.mat
input.rmse(:,:,2,1)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV02/analysis_rmse.mat
input.rmse(:,:,2,2)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV05/analysis_rmse.mat
input.rmse(:,:,2,3)=RMSE_SERIE_G';
load ../DATA/ALLPARAMETERS_VARINGVHIFRECPAR_TRCNV1/analysis_rmse.mat
input.rmse(:,:,2,4)=RMSE_SERIE_G';
load ../DATA/CONTROL_VARINGPARVHIFREC/analysis_rmse.mat
input.rmsectrl(:,:,1)=RMSE_SERIE_G';
load ../DATA/CONTROL_VARINGPARVHIFREC_IMPERFECT/analysis_rmse.mat
input.rmsectrl(:,:,2)=RMSE_SERIE_G';

%Read parameters.tbl
[read_in(1,:) read_in(2,:) read_in(3,:) read_in(4,:) read_in(5,:) read_in(6,:) read_in(7,:) read_in(8,:) read_in(9,:) read_in(10,:) read_in(11,:) ]=textread(PAR_FILE,'%s%s%s%s%s%s%s%s%s%s%s');
aux_name=read_in(1,2:end);
est_par=read_in(3,2:end);
par1d=read_in(4,2:end);

n1d=0;
for i=1:length(est_par);
   if(est_par{i}(1)=='T' & par1d{i}(1) == 'T')
       n1d=n1d+1;
       %aux_name(i)
       %input.parnames{n1d}=(aux_name(i));
       input.parmax(n1d)=str2double(read_in{6,i+1});
       input.parmin(n1d)=str2double(read_in{7,i+1});
   end
end


compara_experimentos_fun(input);


