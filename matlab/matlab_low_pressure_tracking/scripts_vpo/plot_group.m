

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


load_file='../RESULTS/kwbc/GROUP/GROUP_2007060112_L7.mat';
plot_group=1;

%SOME PLOTS TO CONTROL THE SYSTEM BEHAVIOUR

load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end

figure
plot(long,lat)
hold on
a=jet(20);
icolor=20;   
    
jj=plot_group;

   %Corto las trayectorias que pasan por el antimeridiano.
   %=======================================================================
   tmplonanalysis=minlonanalysis(:,jj);
   for kkk=1:(length(tmplonanalysis)-1)
       if(abs(tmplonanalysis(kkk)-tmplonanalysis(kkk+1)) > 180)
           tmplonanalysis(kkk)=NaN;
       end
   end
   tmplonmean=meanlon(:,jj);
   for kkk=1:(length(tmplonmean)-1)
       if(abs(tmplonmean(kkk)-tmplonmean(kkk+1)) > 180)
           tmplonmean(kkk)=NaN;
       end
   end
   
   for kk=1:enssize %INICIO EL LOOP SOBRE LOS MIEMBROS DEL ENSAMBLE.
       tmplon=minlon(kk,:,jj);
   for kkk=1:(length(tmplon)-1)
       if(abs(tmplon(kkk)-tmplon(kkk+1)) > 180)
           tmplon(kkk)=NaN;
       end
   end
   %=======================================================================
   
   plot(tmplon,minlat(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);

   end
   %plot(tmplonmean,tmpstruct.meanlat,'k-','LineWidth',2);
   plot(tmplonanalysis,minlatanalysis(:,jj),'ko--','LineWidth',2);

   %CADA GRUPO VA CON UN COLOR DIFERENTE.
   icolor=icolor+1;
   if(icolor > size(a,1))
       icolor=1;
   end
print('-dpng',['../figuras/grupo' num2str(jj) '.png'])
close(1)

figure
pcolor(trajid(:,:,jj))
print('-dpng',['../figuras/trajid' num2str(jj) '.png'])
close(1)






