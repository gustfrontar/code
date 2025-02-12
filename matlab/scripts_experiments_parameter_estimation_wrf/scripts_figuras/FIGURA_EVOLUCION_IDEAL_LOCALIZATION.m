clear all
close all

load ../EXPERIMENTOS/QFX2DNOZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

parnozlocs=parameter_time_mean;

parnozloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

parzlocs=parameter_time_mean;

parzloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX0DNOZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

parnozloc0ds=parameter_time_mean;

parnozloc0d=parameter_spatial_average;

load ../EXPERIMENTOS/QFX0DZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

parzloc0ds=parameter_time_mean;

parzloc0d=parameter_spatial_average;

figure


subplot(1,2,1)
hold on
plot(parzloc(:,2),'r','LineWidth',3)
plot(parnozloc(:,2),'b','LineWidth',3)
title('Global parameter estimation')
legend('W. VERT. LOC.','W/O VERT. LOC.')
grid on
axis([1 225 0.1 1.5])

subplot(1,2,2)
hold on
plot(parzloc0d(:,2),'r','LineWidth',3)
plot(parzloc0d(:,2),'b','LineWidth',3)
title('Global parameter estimation')
legend('W. VERT. LOC.','W/O VERT. LOC.')
grid on
axis([1 225 0.1 1.5])


