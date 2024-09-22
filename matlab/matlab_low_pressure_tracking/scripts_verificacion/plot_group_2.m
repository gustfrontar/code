close all
clear all


%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


load_file='../RESULTS/ecmf/GROUP_2/GROUP_2008052512_L7.mat';
%load_file='../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_HSUR_EFRIA_2007050112_2007070112.mat';
load(load_file);

enssize=size(group.minlat,1);
%SOME PLOTS TO CONTROL THE SYSTEM BEHAVIOUR



load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end


for igroup=1:size(group.minlat,3)
figure
plot(long,lat)
hold on
a=jet(20);
icolor=20;   
    
jj=igroup;

   %GRAFICO TRAYECTORIA
   
   subplot(2,2,1)
   hold on
   %Corto las trayectorias que pasan por el antimeridiano.
   %=======================================================================
   tmplonanalysis=group.minlonanalysis(:,jj);
   for kkk=1:(length(tmplonanalysis)-1)
       if(abs(tmplonanalysis(kkk)-tmplonanalysis(kkk+1)) > 180)
           tmplonanalysis(kkk)=NaN;
       end
   end
   tmplonmean=group.meanlon(:,jj);
   for kkk=1:(length(tmplonmean)-1)
       if(abs(tmplonmean(kkk)-tmplonmean(kkk+1)) > 180)
           tmplonmean(kkk)=NaN;
       end
   end
   
   for kk=1:enssize %INICIO EL LOOP SOBRE LOS MIEMBROS DEL ENSAMBLE.
       tmplon=group.minlon(kk,:,jj);
   for kkk=1:(length(tmplon)-1)
       if(abs(tmplon(kkk)-tmplon(kkk+1)) > 180)
           tmplon(kkk)=NaN;
       end
   end

   %=======================================================================

   plot(tmplon,group.minlat(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   end
   plot(tmplonmean,group.meanlat(:,jj),'b--','LineWidth',2);
   plot(tmplonanalysis,group.minlatanalysis(:,jj),'ko--','LineWidth',2);


maxlon=max([ max(max(group.minlon(:,:,jj))) max(group.minlonanalysis(:,jj))]);
maxlat=max([ max(max(group.minlat(:,:,jj))) max(group.minlatanalysis(:,jj))]);
minlon=min([ min(min(group.minlon(:,:,jj))) min(group.minlonanalysis(:,jj))]);
minlat=min([ min(min(group.minlat(:,:,jj))) min(group.minlatanalysis(:,jj))]);

axis([minlon-3 maxlon+3 minlat-3 maxlat+3]);

   subplot(2,2,2)
   hold on
   for kk=1:enssize
   plot(group.minanom(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   end
   plot(squeeze(nanmean(group.minanom(:,:,jj))),'b--','LineWidth',2);
   plot(group.minanomanalysis(:,jj),'ko--','LineWidth',2);


   subplot(2,2,3)
   hold on
   for kk=1:enssize
   plot(group.minlap(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   end
   plot(squeeze(nanmean(group.minlap(:,:,jj))),'b--','LineWidth',2);
   plot(group.minlapanalysis(:,jj),'ko--','LineWidth',2);


   subplot(2,2,4)
   hold on
   for kk=1:enssize
   plot(group.uvel(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   end
   plot(squeeze(nanmean(group.uvel(:,:,jj))),'b--','LineWidth',2);
   plot(group.uvelanalysis(:,jj),'ko--','LineWidth',2);

   
   mkdir(['./figures/grupos_2/']);
   print('-dpng',['./figures/grupos_2/grupo1_' num2str(igroup) '.png']);
   close(1)
end

