function [TrajStruct MinStruct]=calc_trajectory_fun_forecast(path_exp,startdate,enddate,timefrec,member)
%Juan Ruiz 2012
%Compute trajectories from forecasts (use grd format)

%CONFIGURATION.............................................................

%config.leadtime=leadtime;      %Requested lead time.
config.timefrec=timefrec;
config.path=path_exp;
config.date_ini=startdate;
config.member=member;
config.date_end=enddate;

%Config parameters for minimum detection.
config.min_pres_tr=1005;  %Un minimo solo sera detectado si su presion es menor que este umbral.
config.low_tr=1005;       %Isobara a partir de la cual se calcula el tama??o de un sistema.
config.distance_tr=0.5e6; %Distancia maxima para que dos minimos puedan ser considerados parte del mismo sistema.
config.pres_tr=3;         %Diferencia de presion maxima para que dos minimos sean considerados el mismo.

config.min_lap_tr=-5;
config.lap_tr=-5;
config.lap_merge_tr=2;

config.resolution=60e3;   %Resolucion del modelo en metros.
config.smooth_field=true;
config.smooth_length=3;
%READ CONSTANT DATA...
%======================================================================

 [xdef ydef zdef]=def_grid_grads();
 config.nx=length(xdef);
 config.ny=length(ydef);
 config.nz=length(zdef);
 [lon_original lat_original]=meshgrid(xdef,ydef);

%READ DATA...
%======================================================================
memberstr=num2str(config.member);
if( config.member < 100 );memberstr=['0' memberstr];end
if( config.member < 10  );memberstr=['0' memberstr];end

count = 1;
cdn=datenum(config.date_ini,'yyyymmddHH');
sdn=cdn;
enddn=datenum(config.date_end,'yyyymmddHH');
cdfolder=cdn;
while ( cdn <= enddn )
%DEFINE PRIOR GRID

lead=(count-1)*config.timefrec;
leadstr=num2str(lead);
if( lead < 10 );leadstr=['0' leadstr];end

filename=[config.path '/typhoon_' datestr(cdn,'yyyy-mm-dd_HH') ':00.dat' ]

%READ DATA
[ U10M , V10M , HR2M , SLP(:,:,count) ]=read_arwpost_nature(filename,config.nx,config.ny);


%STORE SLP AND ITS LAPLACIAND AND THE WIND SPEED
res=nanmean(nanmean(diff(lat_original)));
WIND10M(:,:,count)=sqrt(squeeze(U10M.^2)+squeeze(V10M.^2));


[tmp1,tmp2]=gradient(squeeze(SLP(:,:,count)));
LAPLACIAN(:,:,count)=-divergence(tmp1,tmp2)/(res^2);

cdn=cdn+config.timefrec/24;
count=count+1;
end
%=========================================================================
% USE THE COMPUTED DATA TO GET THE LOCAL MINIMUN OF THE FIELD.
%=========================================================================
for count=1:size(SLP,3)

if (config.smooth_field)
  SLPsmooth=suaviza_fun(squeeze(SLP(:,:,count)),config.smooth_length);
else
  SLPsmooth=squeeze(SLP(:,:,count));
end

[ tmp MASCARA(:,:,count) ] = min_fun( SLPsmooth,lon_original,lat_original,config.min_pres_tr,config.low_tr,config.pres_tr,config.distance_tr);
%[ tmp MASCARA(:,:,count) ] = min_fun( squeeze(LAPLACIAN(:,:,count)),lon_original,lat_original,config.min_lap_tr,config.lap_tr,config.lap_merge_tr,config.distance_tr);
tmp.daten=datenum(config.date_ini,'yyyymmddHH')+(count-1)*config.timefrec/24;

MinStruct(count)=tmp;
end


% for ii=1:1; %count
%    figure
%    plot(MinStruct(ii).minlon,MinStruct(ii).minlat,'o');
%    axis([100 180 0 60])
%    grid on
%     
% end


%=========================================================================
% USE THE COMPUTED MASK TO COMPUTE ADDITIONAL SYSTEM FEATURES
%=========================================================================
for count=1:size(SLP,3)
  for imin=1:MinStruct(count).nminimos

     %Compute mean and min laplacian.
       tmpfield=squeeze(LAPLACIAN(:,:,count));
       tmpmask =squeeze(MASCARA(:,:,count));

       %count
       %imin
       %min(tmpfield(tmpmask==MinStruct(count).id(imin)))

       MinStruct(count).minlap(imin)=min(tmpfield(tmpmask==MinStruct(count).id(imin)));
       MinStruct(count).meanlap(imin)=mean(tmpfield(tmpmask==MinStruct(count).id(imin)));

     %Compute mean and min slp.
       tmpfield=squeeze(SLP(:,:,count));
       tmpmask =squeeze(MASCARA(:,:,count));
       MinStruct(count).minanom(imin)=min(tmpfield(tmpmask==MinStruct(count).id(imin)));
       MinStruct(count).meananom(imin)=mean(tmpfield(tmpmask==MinStruct(count).id(imin)));

     %Compute mean and max wind.
       tmpfield=squeeze(WIND10M(:,:,count));
       tmpmask =squeeze(MASCARA(:,:,count));
       MinStruct(count).maxwind(imin)=max(tmpfield(tmpmask==MinStruct(count).id(imin)));
       MinStruct(count).meanwind(imin)=mean(tmpfield(tmpmask==MinStruct(count).id(imin)));

     %Compute area
       tmpmask =squeeze(MASCARA(:,:,count));
       MinStruct(count).minarea(imin)=sum(sum(tmpmask==MinStruct(count).id(imin)))*((config.resolution/1e3)^2);  %Area en km2
        

  end
  
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








