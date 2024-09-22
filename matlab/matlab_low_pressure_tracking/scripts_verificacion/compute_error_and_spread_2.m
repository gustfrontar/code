close all
clear all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

%La idea de este script es calcular utilizando una funcion el error y el
%spread de diferentes variables en cada uno de los grupos obtenidos.
%Tambien se calcula la probabilidad de existencia del sistema y los rank
%histograms.

config.date_ini='2007040112';          
config.date_end='2007120112';

config.model='ecmf';
config.datadateformat='yyyymmddHH';
config.resultfrec=24; 
config.grouppath=['../RESULTS/' config.model '/GROUP_2/'];
config.outpath=['../RESULTS/' config.model '/ERRORSPREAD/'];
config.outfile=[ config.outpath '/ERROR_SPREAD_' config.date_ini '_' config.date_end '.mat'];
config.forecastlength=7;
%config.hemisferio='SUR';   %Hemisferio que vamos a considerar SUR o NORTE.
%config.estacion='CALIDA';  %Estacion que vamos a considerar (CALIDA O FRIA)
config.mindist=2000e3;     %Minima distancia recorrida por el sistema.

mkdir(config.outpath);
%Will read all the groups comming from the selected model during the
%selected period.

tic
[group]=read_groups_fun_2(config);
tiempo=toc;
fprintf('Time to read the data : %f \n',tiempo);


%VAMOS A FILTRAR ALGUNOS GRUPOS DE ACUERDO CON CIERTOS CRITERIOS...
%==========================================================================

for ig=1:size(group.minlat,3)
   
    index=~isnan(group.meanlon(:,ig));
    
    tmplon=group.meanlon(index,ig);
    tmplat=group.meanlat(index,ig);
    
    abslat=abs(tmplat);
    
    distrecorrida=distll_fun(tmplon(1),tmplat(1),tmplon(end),tmplat(end));
    
    
    if( min(abslat) > 15 && max(abslat) < 80 && distrecorrida > 2000e3 )
    %PEDIMOS QUE EL GRUPO NO ESTE CERCA DEL TROPICO, QUE NO ESTE CERCA DEL POLO
    %Y ADEMAS QUE RECORRA UNA DISTANCIA DE AL MENOS 2000K.
      group.countgroup(ig)=true;
    else
      group.countgroup(ig)=false;
    end
    
end


tic
%Calculo probabilidades, error, spread, etc.
[group]=group_error_spread_fun_2(group);
[group]=compute_pit_histogram_fun(group);
tiempo=toc;
fprintf('Time to compute error, spread, probability, etc : %f \n',tiempo);

%CALCULO EL INDICE DE RESOLUCION PARA LA RELACION SPREAD-ERROR.

for itf=1:size(group.meandistlonerror,1)
[IRLON(:,itf) IRSSLON(:,itf) IPLON(:,itf) PDFLON(:,:,itf) ]=resolution_index_fun(group.distlonspread(itf,:),group.meandistlonerror(itf,:).^2,25,25);
[IRLAT(:,itf) IRSSLAT(:,itf) IPLAT(:,itf) PDFLAT(:,:,itf) ]=resolution_index_fun(group.distlatspread(itf,:),group.meandistlaterror(itf,:).^2,25,25);
[IRTOT(:,itf) IRSSTOT(:,itf) IPTOT(:,itf) PDFTOT(:,:,itf) ]=resolution_index_fun(group.distspread(itf,:),group.meandisterror(itf,:),25,25);

%COMPUTE THE INDEX BASED ON THE INDEAL ERROR
[IRTOTIDEAL(:,itf) IRSSTOTIDEAL(:,itf) IPTOTIDEAL(:,itf) PDFTOTIDEAL(:,:,itf) ]=resolution_index_fun(group.distspread(itf,:),group.idealdisterror(itf,:),25,25);

%COMPUTE THE CORRELATION BETWEEN THE ENSEMBLE SPREAD AND THE ERROR.
tmpindex=(~isnan(group.distlonspread(itf,:)) & ~isnan(group.meandistlonerror(itf,:)));
serie1=group.distlonspread(itf,tmpindex);
serie2=group.meandistlonerror(itf,tmpindex).^2;
rmatrix=corrcoef(serie1,serie2);
CORRCOEFLON(itf)=rmatrix(1,2);
tmpindex=(~isnan(group.distlatspread(itf,:)) & ~isnan(group.meandistlaterror(itf,:)));
serie1=group.distlatspread(itf,tmpindex);
serie2=group.meandistlaterror(itf,tmpindex).^2;
rmatrix=corrcoef(serie1,serie2);
CORRCOEFLAT(itf)=rmatrix(1,2);
tmpindex=(~isnan(group.distspread(itf,:)) & ~isnan(group.meandisterror(itf,:)));
serie1=group.distspread(itf,tmpindex);
serie2=group.meandisterror(itf,tmpindex);
rmatrix=corrcoef(serie1,serie2);
CORRCOEFTOT(itf)=rmatrix(1,2);

end

%AGREGAR ACA LA LLAMADA A LA NUEVA FUNCION PARA EL CALCULO DE LOS PIT
%HISTOGRAMS.


save(config.outfile,'group');



plot_diagnostic_fun(group);







