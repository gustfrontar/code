function [TrajStruct]=calc_trayectory_fun_fnl(path_exp,startdate,leadtime,timefrec)
%Juan Ruiz 2012
%Compute trajectories from fnl analysis (use grd format)

%CONFIGURATION.............................................................

config.leadtime=leadtime;      %Requested lead time.
config.timefrec=timefrec;
config.path=path;
config.date_ini=startdate;
config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+config.leadtime/24,'yyyymmddHH');
folderfrec=6;   %Cada cuantas horas tengo que cambiar la carpeta.

%READ CONSTANT DATA...
%======================================================================

 [xdef ydef zdef]=def_grid_grads();
 config.nx=length(xdef);
 config.ny=length(ydef);
 config.nz=length(zdef);
 [lon_original lat_original]=meshgrid(xdef,ydef);

%READ DATA...
%======================================================================

count = 1;
cdn=datenum(config.date_ini,'yyyymmddHH');
sdn=cdn;
enddn=datenum(config.date_end,'yyyymmddHH');
cdfolder=cdn;
while ( cdn <= enddn )
%DEFINE PRIOR GRID


filename=[path_exp '/gues/control/' datestr(sdn,'yyyymmddHHMM') '/plev/' datestr(cdn,'yyyymmddHHMM') '.dat' ];

[Ut Vt W PSFC QVAPOR HGT RAINC RAINNC GEOPT HEIGHT TK RH RH2 U10M V10M SLPin MCAPE MCIN HFXF QFXF USTF]=read_arwpost_controlforecast_sfflux(filename,config.nx,config.ny,config.nz);

res=nanmean(nanmean(diff(lat_original)));
WIND10M(:,:,count)=sqrt(squeeze(U10M.^2)+squeeze(V10M.^2));
SLP(:,:,count)=SLPin;

[tmp1,tmp2]=gradient(squeeze(SLP(:,:,count)));
LAPLACIAN(:,:,count)=-divergence(tmp1,tmp2)/(res^2);

cdn=cdn+config.timefrec/24;
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








