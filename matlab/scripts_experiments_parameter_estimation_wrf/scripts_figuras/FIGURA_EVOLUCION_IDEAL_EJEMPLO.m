clear all
close all

load ../EXPERIMENTOS/QFX2DNOZLOC40M_IDEAL_CONSTANTPAR_MEMNC/parameters.mat 

pars=parameter_time_mean;

par=parameter_spatial_average;

figure
hold on
set(gca,'FontSize',15)
plot(par(:,2),'r','LineWidth',3)
plot([1 120],[1.8 1.8],'k--','LineWidth',3)

title('Estimated parameter','FontSize',15)
legend('ESTIMATED','TRUE','FontSize', 15)
grid on
axis([1 100 0.8 1.9])




