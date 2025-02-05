%##########################################################################
% ESTE SCRIPT LEE UN ARCHIVO DEL SATÉLITE GOES EN FORMATO NetCDF, LO
% CALIBRA RADIOMETRICAMENTE Y LO CONVIERTE EN VALORES DE TEMPERATURA (ºC)
% EN EL CASO DE LA BANDA 4 (IR TERMICO) Y DE ALBEDO O REFLECTANCIA EN EL
% CASO DE LA BANDA 1 (VIS). FINALMENTE LOS GRAFICA.
%##########################################################################

function []=plot_goes(archivo,escala,banda)

ncload(archivo)

maxlat=max(max(latitude));
minlat=min(min(latitude));
maxlon=max(max(longitude));
minlon=min(min(longitude));

%##########################################################################
%           PROCESAMIENTO DE LA BANDA 4 (10.20 - 11.20 um)
%##########################################################################

if(banda == 4)

%% CONVERSION DE CONTAJES DIGITALES (DN) A RADIANCIA

L = (data/32. - 15.6854)/5.2285; % Unidades: Watt/(m^2-sr-um)

%% CONVERSION DE RADIANCIA A TEMPERATURA DE BRILLO (Se utiliza la inversa
%% de la función de Plank en función del número de onda)

c1 = 1.191066e-05;
c2 = 1.438833;
n = 933.21; %Número de onda en cm^-1
TB= (c2 * n)./log(1 + (c1 * n^3)./L);

%% CONVERSION DE TEMPERATURA DE BRILLO (K) A TEMPERATURA "REAL" (ºC)

alfa = -0.534982;
beta = 1.002693;
gama = -2.667092e-06;
T = alfa + beta*TB + gama*TB.*TB -273.16;

clear TB L data

axesm(...
         'MapProjection','mercator',...
         'MapLatLimit',[minlat maxlat],...
         'MapLonLimit',[minlon maxlon],...
         'labelformat','compass',...
         'grid','on',...
         'fontsize',8,'MLineLocation',10,'PLineLocation',10,'MeridianLabel','on','ParallelLabel','on',...
         'MLabelParallel',-50,'GColor',.8*[1 1 1],'GLineStyle','--');
load worldlo
h=displaym(POline);
set(h,'Color',.5*[1 1 1]);
pcolorm(latitude,longitude,T);

    if(escala == 1)
        v=[-120 -90 -80 -70 -60 -52 -40 -32 -25 -20 -15 -10 -5 0 5 10 15 20 25 30];
        vcol=[55 53 29 26 38 23 44 71  72 73 74 75 76  77 78 79 81 82 83];
        plot_jrcol(v,vcol);
        shading interp
    end

end

%##########################################################################
%               PROCESAMIENTO DE LA BANDA 1 (0.55 - 0.75 um)
%##########################################################################

if(banda == 1) 
 
% CONVERSIÓN DE CONTAJES DIGITALES (DN) A RADIANCIA

L = 0.5521899*(data/32.) - 15.2730; % Unidades: Watt/(m^2-sr-um)

% CONVERSION DE RADIANCIA A REFLECTANCIA

A = 1.92979E-03 * L; 

clear L data

axesm(...
         'MapProjection','mercator',...
         'MapLatLimit',[minlat maxlat],...
         'MapLonLimit',[minlon maxlon],...
         'labelformat','compass',...
         'grid','on',...
         'fontsize',8,'MLineLocation',10,'PLineLocation',10,'MeridianLabel','on','ParallelLabel','on',...
         'MLabelParallel',-50,'GColor',.8*[1 1 1],'GLineStyle','--');
load worldlo
h=displaym(POline);
set(h,'Color',.5*[1 1 1]);
pcolorm(latitude,longitude,A);
 
    if(escala == 2)
        colormap(bone);
        shading interp
    end
end
 
%########################################################################## 

 
 
 
 
 