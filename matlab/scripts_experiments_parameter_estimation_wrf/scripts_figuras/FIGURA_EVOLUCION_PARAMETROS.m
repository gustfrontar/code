clear all
close all

load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/parameters.mat 

paraveqfx0dnozloc=parameter_time_mean;
parvarqfx0dnozloc=parameter_time_mean;

partsqfx0dnozloc=parameter_spatial_average;

%load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/parameters.mat 

%paraveqfx0dzloc=parameter_time_mean;
%parvarqfx0dzloc=parameter_time_mean;

%partsqfx0dzloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/parameters.mat 

paraveqfx2dnozloc=parameter_time_mean;
parvarqfx2dnozloc=parameter_time_mean;

partsqfx2dnozloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/parameters.mat 

paraveqfx2dzloc=parameter_time_mean;
parvarqfx2dzloc=parameter_time_mean;

partsqfx2dzloc=parameter_spatial_average;

figure

%subplot(1,2,1)
hold on
plot(partsqfx0dnozloc(:,2),'r','LineWidth',3)
%plot(partsqfx0dzloc(:,2),'b','LineWidth',3)
plot(partsqfx2dnozloc(:,2),'b','LineWidth',3)
plot(partsqfx2dzloc(:,2),'g','LineWidth',3)
title('Global parameter estimation')
legend('0D NO V. LOC.','2D NO V. LOC.','2D V. LOC.')
grid on
axis([1 225 0.1 1.5])
% subplot(1,2,2)
% hold on
% plot(partsqfx2dnozloc(:,2),'r','LineWidth',3)
% plot(partsqfx2dzloc(:,2),'b','LineWidth',3)
% legend('W/O VERT. LOC.','W. VERT. LOC.')
% title('2D parameter estimation')
% axis([1 225 0.1 1.5])
% grid on

