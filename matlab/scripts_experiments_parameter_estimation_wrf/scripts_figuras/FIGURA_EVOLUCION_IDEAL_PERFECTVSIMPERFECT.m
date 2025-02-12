clear all
close all

load ../EXPERIMENTOS/QFX2DNOZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

parperfects=parameter_time_mean;

parperfect=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DNOZLOC40M_IDEAL_CONSTANTPARIMOD_MEMNC/parameters.mat 

parimperfects=parameter_time_mean;

parimperfect=parameter_spatial_average;

figure

subplot(1,2,1)
hold on
plot(parperfect(:,2),'r','LineWidth',3)
plot(parimperfect(:,2),'b','LineWidth',3)
title('Global parameter estimation')
legend('PERFECT MODEL','IMPERFECT MODEL')
grid on
axis([1 225 0.1 1.5])


