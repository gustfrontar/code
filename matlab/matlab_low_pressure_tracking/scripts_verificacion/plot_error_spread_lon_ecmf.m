clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.


infile='../RESULTS/ecmf/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/ecmf/ERRORSPREAD/DISTERRORLON_2007040112_2010040112.mat';


load(infile);
enssize=size(group.minlat,1);
flength=size(group.minlat,2);



%Calculo para todos los grupos.
tmpmean=(group.meandistlonerror);
tmpctrl=(group.ctrldistlonerror);
tmpmemb=group.nummembers;
tmpsprd=group.distlonspread;
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_total=sqrt(nanmean(tmpmean.^2,2));
ctrldist_total=sqrt(nanmean(tmpctrl.^2,2));
distsprd_total=sqrt(nanmean(tmpsprd.^2,2));

meanbias_total=nanmean(tmpmean,2);
ctrlbias_total=nanmean(tmpctrl,2);


%Para el hemisferio sur
index=group.shemi==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemi=sqrt(nanmean(tmpmean.^2,2));
ctrldist_shemi=sqrt(nanmean(tmpctrl.^2,2));
distsprd_shemi=sqrt(nanmean(tmpsprd.^2,2));

meanbias_shemi=nanmean(tmpmean,2);
ctrlbias_shemi=nanmean(tmpctrl,2);

%Para el hemisferio norte
index=group.nhemi==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemi=sqrt(nanmean(tmpmean.^2,2));
ctrldist_nhemi=sqrt(nanmean(tmpctrl.^2,2));
distsprd_nhemi=sqrt(nanmean(tmpsprd.^2,2));

meanbias_nhemi=nanmean(tmpmean,2);
ctrlbias_nhemi=nanmean(tmpctrl,2);

%Para la estacion calida
index=group.calida==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb  < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_calida=sqrt(nanmean(tmpmean.^2,2));
ctrldist_calida=sqrt(nanmean(tmpctrl.^2,2));
distsprd_calida=sqrt(nanmean(tmpsprd.^2,2));

meanbias_calida=nanmean(tmpmean,2);
ctrlbias_calida=nanmean(tmpctrl,2);

%Para la estacion fria
index=group.fria==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_fria=sqrt(nanmean(tmpmean.^2,2));
ctrldist_fria=sqrt(nanmean(tmpctrl.^2,2));
distsprd_fria=sqrt(nanmean(tmpsprd.^2,2));

meanbias_fria=nanmean(tmpmean,2);
ctrlbias_fria=nanmean(tmpctrl,2);


%FIGURA ERRORES POR TOTAL, POR HEMISFERIO Y POR ESTACION.
figure
subplot(1,3,1)
hold on
plot(meandist_total,'k-','LineWidth',2);
plot(ctrldist_total,'k--','LineWidth',2);
plot(distsprd_total,'k-.','LineWidth',2);

%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Total distance error');legend('MEAN ERROR','CTRL ERROR','SPREAD');

subplot(1,3,2)
hold on
plot(meandist_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(distsprd_shemi,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_nhemi,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Distance error by hemisphere');legend('SH MEAN ERROR','NH MEAN ERROR','SH CTRL ERROR','NH CTRL ERROR','SH SPREAD','NH SPREAD');


subplot(1,3,3)
hold on
plot(meandist_calida,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_fria,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_calida,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_fria,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(distsprd_calida,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_fria,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Distance error by season');legend('WARM MEAN ERROR','COLD MEAN ERROR','WARM CTRL ERROR','COLD CTRL ERROR','WARM SPREAD','COLD SPREAD');

%FIGURA BIAS, POR HEMISFERIO Y POR ESTACION.
figure
subplot(1,3,1)
hold on
plot(meanbias_total,'k-','LineWidth',2);
plot(ctrlbias_total,'k--','LineWidth',2);

%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Longitude bias');legend('MEAN','CTRL');

subplot(1,3,2)
hold on
plot(meanbias_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meanbias_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrlbias_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrlbias_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Longitude bias');legend('SH MEAN','NH MEAN','SH CTRL','NH CTRL');


subplot(1,3,3)
hold on
plot(meanbias_calida,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meanbias_fria,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrlbias_calida,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrlbias_fria,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Longitude bias');legend('WARM MEAN','COLD MEAN','WARM CTRL','COLD CTRL');


%OTRA FIGURA PARA VER LOS ERRORES POR HEMISFERIO Y POR ESTACION (CALIDA O
%FRIA).

%Para el hemisferio sur y estacion calida
index=group.shemi==1 & group.calida==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemic=sqrt(nanmean(tmpmean.^2,2));
ctrldist_shemic=sqrt(nanmean(tmpctrl.^2,2));
distsprd_shemic=nanmean(tmpsprd,2);

%Para el hemisferio norte y estacion calida
index=group.nhemi==1 & group.calida == 1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemic=sqrt(nanmean(tmpmean.^2,2));
ctrldist_nhemic=sqrt(nanmean(tmpctrl.^2,2));
distsprd_nhemic=nanmean(tmpsprd,2);

%Para el hemisferio sur y estacion fria
index=group.shemi==1 & group.fria ==1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb  < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemif=sqrt(nanmean(tmpmean.^2,2));
ctrldist_shemif=sqrt(nanmean(tmpctrl.^2,2));
distsprd_shemif=nanmean(tmpsprd,2);

%Para el hemisferio norte y la estacion fria.
index=group.nhemi==1 & group.fria == 1;
tmpmean=group.meandistlonerror(:,index);
tmpctrl=group.ctrldistlonerror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distlonspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemif=sqrt(nanmean(tmpmean.^2,2));
ctrldist_nhemif=sqrt(nanmean(tmpctrl.^2,2));
distsprd_nhemif=nanmean(tmpsprd,2);

figure
subplot(1,2,1)

hold on
plot(meandist_shemic,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_shemif,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_shemic,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_shemif,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

plot(distsprd_shemic,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_shemif,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('SOUTHERN HEMISPHERE');legend('WARM MEAN','COLD MEAN','WARM CTRL','COLD CTRL','WARM SPREAD','COLD SPREAD');


subplot(1,2,2)
hold on
plot(meandist_nhemic,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_nhemif,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_nhemic,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_nhemif,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

plot(distsprd_nhemic,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_nhemif,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('NORTHERN HEMISPHERE');legend('WARM MEAN','COLD MEAN','WARM CTRL','COLD CTRL','WARM SPREAD','COLD SPREAD');

clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
save(outfile)



