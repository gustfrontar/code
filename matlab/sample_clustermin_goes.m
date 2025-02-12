%Fecha modificacion 22/06/2014

clear all; close all; clc

files=dir('../../data/ir/140405/1404051145G13I04');
pathsat='../../data/ir/140405/';
pathnav='../../data/nav/';
pathcalib='../../data/calib/';

fileir=files(1).name;

[TB latsat lonsat] = readgoes(pathsat,pathnav,pathcalib,fileir);

latsat(latsat>-21)=NaN;
latsat(latsat<-43)=NaN;
lonsat(lonsat>-50)=NaN;
lonsat(lonsat<-72)=NaN;


TBsuav=suaviza_fun(TB,4);
TBsuav(TBsuav==0)=NaN;
TBsuav=TBsuav+randn(size(TBsuav))*1e-2;

UMB_TB    = 220;
UMB_SIS   = 235;
UMBRAL_ASOCIACION = 5;
UMBRAL_CERCANIA = 0.5e6;

[ minstruct MASCARA_SIS ] = min_fun( TBsuav,lonsat,latsat,UMB_TB,UMB_SIS,UMBRAL_ASOCIACION,UMBRAL_CERCANIA );

pcolor(lonsat,latsat,MASCARA_SIS);shading flat
%=========================================================================%

figure('Visible','on','Position',[150 150 1200 600],'Color','white')

%=========================================================================%
% IMAGEN IR
%=========================================================================%

subplot(1,2,1)
box('on')
hold('all')

pcolor(lonsat,latsat,TB-273)
shading flat
caxis([-80 40])
colorbar
load('mapas.mat')
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0 0 0]);
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0 0 0]);
axis([-65 -55 -40 -30])
set(gca,'PlotBoxAspectratio',[1 1 1],'LineWidth',1.5,'Layer','Top','FontSize',12)

%=========================================================================%
% IMAGEN SEGMENTADA
%=========================================================================%

subplot(1,2,2)
box('on')
hold('all')

pcolor(lonsat,latsat,MASCARA_SIS)
shading flat
colorbar
plot(minstruct.minlon,minstruct.minlat,'ok')
load('mapas.mat')
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0 0 0]);
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0 0 0]);
axis([-65 -55 -40 -30])
set(gca,'PlotBoxAspectratio',[1 1 1],'LineWidth',1.5,'Layer','Top','FontSize',12)

%=========================================================================%
