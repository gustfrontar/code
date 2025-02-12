clear all
close all

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/parameters.mat 

paraveqfx2dnozloc=parameter_time_mean;
parvarqfx2dnozloc=parameter_time_mean;

partsqfx2dnozloc=parameter_spatial_average;


figure


hold on
set(gca,'FontSize',15)
plot(partsqfx2dnozloc(:,2),'b','LineWidth',3)
plot([1 300],[1 1],'k--','LineWidth',3)
legend('Estimated','WRF default','FontSize',15)
title('2D parameter estimation','FontSize',15)
axis([1 225 0.1 1.5])
grid on

