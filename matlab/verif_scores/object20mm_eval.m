clear all
close all

%Evaluation of bias in the forecast.


load model_data.mat
load lluvia_obs_hires.mat
load lat_lon.mat


nforecast=size(lluvia_obs,3);
ndays=size(lluvia_obs,4);

%Compare PDF.

umbral=20;  %Thresholds to compute FSS.

forecast=squeeze(lluvia_modelo(:,:,6,7));
observation=squeeze(lluvia_obs(:,:,6,7));

%STEP ONE: SMOOTH FIELDS TO GENERATE MASKS.
forecast_smooth=suaviza_fun(forecast,40);
observation_smooth=suaviza_fun(observation,40);

%STEP TWO: GENERATE MASKS ACCORDING TO SELECTED THRESHOLD.
forecast_mask=forecast_smooth > umbral;
observation_mask=observation_smooth > umbral;

%STEP THREE: IDENTIFY OBJECTS AND COMPUTE THEIR CHARACTERISTICS.
%Compute mean, max, position, precipitation weighted position, eof
%orientation and aspect ratio.

[forecast_system_struct forecast_mask_id]=cluster_fun(forecast_mask,forecast,LON,LAT);
[observation_system_struct observation_mask_id]=cluster_fun(observation_mask,observation,LON,LAT);

%This step can be performed independently and then statistics of system
%properties can be derived from the forecast and from the observations to
%asses model performance in representing the "object climatology".

%STEP FOUR: Matching,
%This is certainly the most difficult and "less objective" part of the algoritm.
%A criteria has to be defined in order to decide when a particular object
%in the forecast corresponds to an object in the observed field. This can
%be particularly difficult even for a human been based on subjective
%appretiation. 
%In this case we will apply a simple matching function based on a distance
%criteria (Davies et al. 2006 MWR).

[forecast_system_struct observation_system_struct]=simple_match_fun(forecast_system_struct,observation_system_struct);

%FIGURES:

%COMPARACION VISUAL DE 2 CAMPOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,forecast)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')

subplot(1,2,2)

pcolor(LON,LAT,observation)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION')

%CAMPOS SUAVIZADOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,forecast_smooth)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')

subplot(1,2,2)

pcolor(LON,LAT,observation_smooth)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION')


%MASCARA CON SISTEMAS IDENTIFICADOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,forecast_mask_id)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-0.5 0.5 1.5 2.5 3.5],[2 24 28 47])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')
contour(LON,LAT,observation_mask_id,'b-','LineWidth',2);

subplot(1,2,2)

pcolor(LON,LAT,observation_mask_id)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-0.5 0.5 1.5 2.5 3.5],[2 24 28 47])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION')
contour(LON,LAT,forecast_mask_id,'b-','LineWidth',2);

%MASCARA Y ATRIBUTOS DE LOS SISTEMAS.
%==========================================================================
figure


carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,forecast_mask_id)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-0.5 0.5 1.5 2.5 3.5],[2 24 28 47])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')

for ii=1:forecast_system_struct.nsistemas

    %Ploteo los centroides
    lon=forecast_system_struct.systems(ii).centroidlon;
    lat=forecast_system_struct.systems(ii).centroidlat;
    plot(lon,lat,'ko','MarkerSize',8,'MarkerFaceColor',[0 0 0])
    eof=forecast_system_struct.systems(ii).eof;
    quiver(lon,lat,eof(1,1),eof(2,1),'LineWidth',2,'Color','k')
    quiver(lon,lat,eof(1,2),eof(2,2),'LineWidth',2,'Color','k')
    sizesis=forecast_system_struct.systems(ii).gridsize;
    text(lon,lat,['  Size=' num2str(sizesis*16) 'km^2 '],...
     'HorizontalAlignment','left','FontSize',14)
    maxpp=forecast_system_struct.systems(ii).maxpp;
    text(lon,lat-0.5,['  Max pp=' num2str(maxpp) 'mm '],...
     'HorizontalAlignment','left','FontSize',14)
    meanpp=forecast_system_struct.systems(ii).meanpp;
    text(lon,lat-1.0,['  Mean pp=' num2str(meanpp) 'mm '],...
     'HorizontalAlignment','left','FontSize',14)
    aspectratio=forecast_system_struct.systems(ii).aspectratio;
    text(lon,lat-1.5,['  Aspect ratio=' num2str(aspectratio) ' '],...
     'HorizontalAlignment','left','FontSize',14)
    orientation=forecast_system_struct.systems(ii).orientation;
    text(lon,lat-2.0,['  Orientation=' num2str(orientation*360/6.28) 'deg'],...
    'HorizontalAlignment','left','FontSize',14)
end

figure
pcolor(LON,LAT,observation_mask_id)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-0.5 0.5 1.5 2.5 3.5],[2 24 28 47])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATIONS')

for ii=1:observation_system_struct.nsistemas

    %Ploteo los centroides
    lon=observation_system_struct.systems(ii).centroidlon;
    lat=observation_system_struct.systems(ii).centroidlat;
    plot(lon,lat,'ko','MarkerSize',8,'MarkerFaceColor',[0 0 0])
    eof=observation_system_struct.systems(ii).eof;
    quiver(lon,lat,eof(1,1),eof(2,1),'LineWidth',2,'Color','k')
    quiver(lon,lat,eof(1,2),eof(2,2),'LineWidth',2,'Color','k')
    sizesis=observation_system_struct.systems(ii).gridsize;
    text(lon,lat,['  Size=' num2str(sizesis*16) 'km^2 '],...
     'HorizontalAlignment','left','FontSize',14)
    maxpp=observation_system_struct.systems(ii).maxpp;
    text(lon,lat-0.5,['  Max pp=' num2str(maxpp) 'mm '],...
     'HorizontalAlignment','left','FontSize',14)
    meanpp=observation_system_struct.systems(ii).meanpp;
    text(lon,lat-1.0,['  Mean pp=' num2str(meanpp) 'mm '],...
     'HorizontalAlignment','left','FontSize',14)
    aspectratio=observation_system_struct.systems(ii).aspectratio;
    text(lon,lat-1.5,['  Aspect ratio=' num2str(aspectratio) ' '],...
     'HorizontalAlignment','left','FontSize',14)
    orientation=observation_system_struct.systems(ii).orientation;
    text(lon,lat-2.0,['  Orientation=' num2str(orientation*360/6.28) 'deg'],...
    'HorizontalAlignment','left','FontSize',14)
end


%Plot system associations. For each forecasted system plot its
%corresponding associated observation (if any).
%==========================================================================

for ii=1:forecast_system_struct.nsistemas
figure

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;
%Plot current system.
pcolor(LON,LAT,double(forecast_mask_id==ii))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-0.5 0.5 1.5],[2 28])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')

    %Ploteo los centroides
    lon=forecast_system_struct.systems(ii).centroidlon;
    lat=forecast_system_struct.systems(ii).centroidlat;
    plot(lon,lat,'ko','MarkerSize',8,'MarkerFaceColor',[0 0 0])
    
    if(forecast_system_struct.systems(ii).matched)
    obssystem=forecast_system_struct.systems(ii).matchedsys;
    lonobs=observation_system_struct.systems(obssystem).centroidlon;
    latobs=observation_system_struct.systems(obssystem).centroidlat;
    %Plot the matched observed system.
      contour(LON,LAT,double(observation_mask_id==ii),'b','LineWidth',2);
      plot([lon lonobs],[lat latobs],'k-','LineWidth',2);
      text(lon,lat,['  Dist factor=' num2str(forecast_system_struct.systems(ii).distance_factor) ' '],...
     'HorizontalAlignment','left','FontSize',14) 
    end
end
