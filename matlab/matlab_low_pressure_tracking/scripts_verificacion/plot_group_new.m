close all
clear all


%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


load_file='../RESULTS/kwbc/GROUP_2/GROUP_2007070112_L7.mat';
load(load_file);

%enssize=size(group.minlat,1);
%SOME PLOTS TO CONTROL THE SYSTEM BEHAVIOUR



load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end


for igroup=1:group.ngroup

figure
plot(long,lat)
hold on
a=jet(20);
icolor=20;   
    
jj=igroup;
ntraj=size(group.trajinfo(jj).minlat,1);
%ntrajanalysis=size(group.trajinfo(jj).minlatanalysis,1);

   %GRAFICO TRAYECTORIA
   
   %subplot(2,2,1)
   hold on
   %Corto las trayectorias que pasan por el antimeridiano.
   %=======================================================================
   %tmplonanalysis=group.trajinfo(jj).minlonanalysis;
   %for kk=1:ntrajanalysis %INICIO EL LOOP SOBRE LOS MIEMBROS DEL ENSAMBLE.
   %for kkk=1:(length(tmplonanalysis(kk,:))-1)
   %    if(abs(tmplonanalysis(kk,kkk)-tmplonanalysis(kk,kkk+1)) > 180)
   %        tmplonanalysis(kk,kkk)=NaN;
   %    end
   %end
   %end
   
   tmplonmean=group.meanlon(:,jj);
   for kkk=1:(length(tmplonmean)-1)
       if(abs(tmplonmean(kkk)-tmplonmean(kkk+1)) > 180)
           tmplonmean(kkk)=NaN;
       end
   end
   
   tmplon=group.trajinfo(jj).minlon;
   for kk=1:ntraj %INICIO EL LOOP SOBRE LOS MIEMBROS DEL ENSAMBLE.
   for kkk=1:(length(tmplon(kk,:))-1)
       if(abs(tmplon(kk,kkk)-tmplon(kk,kkk+1)) > 180)
           tmplon(kk,kkk)=NaN;
       end
   end
   end

   %=======================================================================

   plot(tmplon',group.trajinfo(jj).minlat','-','Color',a(icolor,:),'LineWidth',1);
   
   plot(tmplonmean,group.meanlat(:,jj),'b--','LineWidth',2);
   %plot(tmplonanalysis',group.trajinfo(jj).minlatanalysis','ko--','LineWidth',2);

maxlon=max(max(group.trajinfo(jj).minlon));
maxlat=max(max(group.trajinfo(jj).minlat));
minlon=min(min(group.trajinfo(jj).minlon));
minlat=min(min(group.trajinfo(jj).minlat));
%maxlon=max([ max(max(group.minlon(:,:,jj))) max(group.minlonanalysis(:,jj))]);
%maxlat=max([ max(max(group.minlat(:,:,jj))) max(group.minlatanalysis(:,jj))]);
%minlon=min([ min(min(group.minlon(:,:,jj))) min(group.minlonanalysis(:,jj))]);
%minlat=min([ min(min(group.minlat(:,:,jj))) min(group.minlatanalysis(:,jj))]);
if(~isnan(minlon))
axis([minlon-3 maxlon+3 minlat-3 maxlat+3]);
end

   %subplot(2,2,2)
   %hold on
   %for kk=1:ntraj
   %plot(group.minanom(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   %end
   %plot(squeeze(nanmean(group.minanom(:,:,jj))),'b--','LineWidth',2);
   %plot(group.minanomanalysis(:,jj),'ko--','LineWidth',2);


   %subplot(2,2,3)
   %hold on
   %for kk=1:enssize
   %plot(group.minlap(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   %end
   %plot(squeeze(nanmean(group.minlap(:,:,jj))),'b--','LineWidth',2);
   %plot(group.minlapanalysis(:,jj),'ko--','LineWidth',2);


   %subplot(2,2,4)
   %hold on
   %for kk=1:enssize
   %plot(group.uvel(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);
   %end
   %plot(squeeze(nanmean(group.uvel(:,:,jj))),'b--','LineWidth',2);
   %plot(group.uvelanalysis(:,jj),'ko--','LineWidth',2);


   %subplot(2,2,4)
   
   mkdir(['./figures/grupos_new/']);
   print('-dpng',['./figures/grupos_new/grupo1_' num2str(igroup) '.png']);
   close(1)
end

