function [TrajStruct]=calc_trayectory_fun_fnl(path,startdate,leadtime,timefrec)
%Juan Ruiz 2012
%Compute trajectories from fnl analysis (use netcdf met_em format)

%CONFIGURATION.............................................................

config.leadtime=leadtime;      %Requested lead time.
config.timefrec=timefrec;
config.path=path;
config.date_ini=startdate;
config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+config.leadtime/24,'yyyymmddHH');



%READ DATA...
%======================================================================

count = 1;
cdn=datenum(config.date_ini,'yyyymmddHH');
enddn=datenum(config.date_end,'yyyymmddHH');
while ( cdn <= enddn )
%DEFINE PRIOR GRID
filename=[path 'met_em.d01.' datestr(cdn,'yyyy') '-' datestr(cdn,'mm') '-' datestr(cdn,'dd') '_' datestr(cdn,'HH') ':00:00.nc'];
if(count==1)
ncload(filename,'XLAT_M','XLONG_M','XLAT_U','XLAT_V','XLONG_U','XLONG_V');
lon_original=XLONG_M;
lat_original=XLAT_M;
end

res=nanmean(nanmean(diff(lat_original)));
%No hay U10 y V10 en los met_em. (Uso UU y VV en el nivel mas bajo)
ncload(filename,'UU','VV','PMSL');
tmpU=squeeze(UU(1,:,:));
tmpV=squeeze(VV(1,:,:));
%Interp wind variables to the pressure grid.
U=interp2(XLONG_U,XLAT_U,tmpU,XLONG_M,XLAT_M,'linear',0);
V=interp2(XLONG_V,XLAT_V,tmpV,XLONG_M,XLAT_M,'linear',0);
WIND10M(:,:,count)=sqrt(squeeze(U.^2)+squeeze(V.^2));

%Compute slp
SLP(:,:,count)=PMSL/100;  %SLP is already computed :)


[tmp1,tmp2]=gradient(squeeze(SLP(:,:,count)));
LAPLACIAN(:,:,count)=-divergence(tmp1,tmp2)/(res^2);

cdn=cdn+3/24;
count=count+1;
end
%=========================================================================
% USE THE COMPUTED DATA TO GET THE LOCAL MINIMUN OF THE FIELD.
%=========================================================================
for count=1:size(SLP,3)
tmp=min_fun(squeeze(SLP(:,:,count)),squeeze(LAPLACIAN(:,:,count)),squeeze(WIND10M(:,:,count)),lon_original,lat_original,false);
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








