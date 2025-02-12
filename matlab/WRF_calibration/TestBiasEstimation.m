clear all
close all

%==========================================================================
% Este script realiza una verificacion "Offline" de los pronosticos de
% viento y potencia y ademas compara los resultados con los obtenidos a
% partir de un pronostico calibrado estadisticamente.
%==========================================================================

%Cargamos un conjunto de datos de pronosticos y observaciones. Este archivo
%contiene una estructura Val que tiene 4 arrays (pronosticos del WRF para
%velocidad y direccion y observaciones del viento medio del parque para
%velocidad y direccion)

load datos_val.mat;

current_date=280;
current_lead=250;
delta_t=60;

VelForDb=reshape(Val.WRFVel(current_date-delta_t:current_date+delta_t,current_lead-3:current_lead+3),[(delta_t*2+1)*(3*2+1) 1] );
VelObsDb=reshape(Val.ObsVel(current_date-delta_t:current_date+delta_t,current_lead-3:current_lead+3),[(delta_t*2+1)*(3*2+1) 1] );
DirForDb=reshape(Val.WRFDir(current_date-delta_t:current_date+delta_t,current_lead-3:current_lead+3),[(delta_t*2+1)*(3*2+1) 1] );
DirObsDb=reshape(Val.ObsDir(current_date-delta_t:current_date+delta_t,current_lead-3:current_lead+3),[(delta_t*2+1)*(3*2+1) 1] );

%DatesDb=Val.datetotal(current_date-delta_t:current_date+delta_t,:);

indexdir=0;

for idir=0:60:300;
    indexdir=indexdir+1;
    
%    for ivel=0:5:25
      
      Index= DirForDb > idir & DirForDb < idir+60;  
      Obs=VelObsDb(Index);
      For=VelForDb(Index);
        
      BiasVel(indexdir)=mean( For - Obs );
      BiasNum(indexdir)=length(Obs);

%    end
end