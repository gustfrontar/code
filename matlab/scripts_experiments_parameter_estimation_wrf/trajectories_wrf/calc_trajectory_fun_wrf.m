function [TrajStruct]=calc_trayectory_fun_wrf(filename,startdate,leadtime,timefrec)
%Juan Ruiz 2012

%CONFIGURATION.............................................................

config.leadtime=leadtime;      %Requested lead time.
config.timefrec=timefrec;
config.filename=filename;
config.date_ini=startdate;
config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+config.leadtime/24,'yyyymmddHH');



%READ DATA...
%======================================================================

%DEFINE PRIOR GRID
ncload(config.filename,'XLAT','XLONG');
lon_original=squeeze(XLONG(1,:,:));
lat_original=squeeze(XLAT(1,:,:));

res=nanmean(nanmean(diff(lat_original)));

ncload(config.filename,'U10','V10');
WIND10M=sqrt(U10.^2+V10.^2);

%Compute slp
ncload(config.filename,'T','PB','P','PH','PHB','QVAPOR')
Z=(PH+PHB)/9.81;
P=(P+PB);
T=T+290;
%Get slp in hPa.
[SLP]=calc_slp_fun(P,Z,T,QVAPOR);

%pcolor(SLP(:,:,1))

for it=1:size(SLP,3);
[tmp1,tmp2]=gradient(squeeze(SLP(:,:,it)));
LAPLACIAN(:,:,it)=-divergence(tmp1,tmp2)/(res^2);
end

%=========================================================================
% USE THE COMPUTED DATA TO GET THE LOCAL MINIMUN OF THE FIELD.
%=========================================================================
for count=1:size(SLP,3)
tmp=min_fun(squeeze(SLP(:,:,count)),squeeze(LAPLACIAN(:,:,count)),squeeze(WIND10M(count,:,:)),lon_original,lat_original,false);
tmp.daten=datenum(config.date_ini,'yyyymmddHH')+(count-1)*config.timefrec/24;
MinStruct(count)=tmp;
end

%======================================================================
%Calculamos las trayectorias intentando asociar los sistemas presentes en
%el tiempo T con aquellos presentes en el T+1
%==========================================================================
  tic
  [TrajStruct]=trajectory_fun_wrf(config,MinStruct);
  tiempo=toc;
  fprintf('TIME TO GET THE TRAJECTORIES=%f\n',tiempo)

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
end








