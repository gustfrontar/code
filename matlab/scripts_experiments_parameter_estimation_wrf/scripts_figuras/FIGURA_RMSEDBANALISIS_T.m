clear all
close all


load ../EXPERIMENTOS/CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

BIASTCTRL=RMSEDEBIASU;
%BIASUCTRL=BIASU;
%BIASVCTRL=BIASV;
%BIASQCTRL=BIASQ;
%BIASHCTRL=BIASH;


load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST0DNZ=RMSEDEBIASU;
%BIASU0DNZ=BIASU;
%BIASV0DNZ=BIASV;
%BIASQ0DNZ=BIASQ;
%BIASH0DNZ=BIASH;

load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST0D=RMSEDEBIASU;
%BIASU0D=BIASU;
%BIASV0D=BIASV;
%BIASQ0D=BIASQ;
%BIASH0D=BIASH;

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST2DNZ=RMSEDEBIASU;
%BIASU2DNZ=BIASU;
%BIASV2DNZ=BIASV;
%BIASQ2DNZ=BIASQ;
%BIASH2DNZ=BIASH;

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST2D=RMSEDEBIASU;
%BIASU2D=BIASU;
%BIASV2D=BIASV;
%BIASQ2D=BIASQ;
%BIASH2D=BIASH;


[xdef ydef zdef]=def_grid_grads();
[lonwrf latwrf]=meshgrid(xdef,ydef);
load coast

ilevel=6;  %6 is 850.
figure
subplot(3,2,1)

hold on
pcolor(lonwrf,latwrf,BIASTCTRL(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('CONTROL','FontSize',15)

subplot(3,2,3)

hold on
pcolor(lonwrf,latwrf,BIAST0D(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('0D with vert. loc.','FontSize',15)

subplot(3,2,4)
hold on
pcolor(lonwrf,latwrf,BIAST0DNZ(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('0D with vert. loc.','FontSize',15)

subplot(3,2,5)

hold on
pcolor(lonwrf,latwrf,BIAST2D(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('2D with vert. loc.','FontSize',15)

subplot(3,2,6)
hold on
pcolor(lonwrf,latwrf,BIAST2DNZ(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('2D with vert. loc.','FontSize',15)


ilevel=12;  %12 is 300.
figure
subplot(3,2,1)

hold on
pcolor(lonwrf,latwrf,BIASTCTRL(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('CONTROL','FontSize',15)

subplot(3,2,3)

hold on
pcolor(lonwrf,latwrf,BIAST0D(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('0D with vert. loc.','FontSize',15)

subplot(3,2,4)
hold on
pcolor(lonwrf,latwrf,BIAST0DNZ(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('0D with vert. loc.','FontSize',15)

subplot(3,2,5)

hold on
pcolor(lonwrf,latwrf,BIAST2D(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('2D with vert. loc.','FontSize',15)

subplot(3,2,6)
hold on
pcolor(lonwrf,latwrf,BIAST2DNZ(:,:,ilevel));shading flat
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([-2 -1.5 -1 -0.5 -0.25 0.25 0.5 1.0 1.5 2.0],[49 47 45 43 2 23 25 27 29],2)
plot(long,lat,'-k','LineWidth',2);
title('2D with vert. loc.','FontSize',15)
