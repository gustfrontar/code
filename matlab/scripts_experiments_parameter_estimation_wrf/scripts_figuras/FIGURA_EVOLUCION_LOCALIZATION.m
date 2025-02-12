clear all
close all

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/parameters.mat 

parnozlocs=parameter_time_mean;

parnozloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/parameters.mat 

parzlocs=parameter_time_mean;

parzloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/parameters.mat 

parnozloc0ds=parameter_time_mean;

parnozloc0d=parameter_spatial_average;

%load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/parameters.mat 

%parzloc0ds=parameter_time_mean;

%parzloc0d=parameter_spatial_average;

figure


%subplot(1,2,1)
hold on
set(gca,'FontSize',15)
plot(parzloc(:,2),'r','LineWidth',3)
plot(parnozloc(:,2),'b','LineWidth',3)
plot(parnozloc0d(:,2),'g','LineWidth',3)
%title('Global parameter estimation')
legend('2D VERT. LOC.','2D WITHOUT VERT. LOC.','0D WITHOUT VERT. LOC.','FontSize',15)
grid on
axis([1 225 0.1 1.5])


