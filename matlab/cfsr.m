clear all
close all
archivocfsr='pgbh00.gdas.20030117_00.grb2';
% creo el ncgeodataset
nc=ncgeodataset(['/home/patito/DATA/CFSR/' archivocfsr ]);

%listo las variables
nc.variables

%creo una geovariable
dirvar=nc.geovariable('Total_precipitation_surface_0_Hour_Accumulation');

% solo considero el primer tiempo ( es medio estupido esto porque el
% archivo tiene un solo tiempo pero bue......)
dir=dirvar.data(1,:,:);

% creo la grilla para el tiempo elegido
g=dirvar.grid_interop(1,:,:);

% en "teoria" ploteo ( que es pcolorjw?????)
pcolorjw(g.lon,g.lat,dir);
