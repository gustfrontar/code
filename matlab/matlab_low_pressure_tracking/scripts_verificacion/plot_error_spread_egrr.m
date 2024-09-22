%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

infile='../RESULTS/egrr/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/egrr/ERRORSPREAD/DISTERROR_2007040112_2010040112.mat';


load(infile);

enssize=size(group.minlat,1);
flength=size(group.minlat,2);

group.minlap=[];
group.minanom=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];

%Calculo para todos los grupos.
tmpmean=group.meandisterror;
tmpctrl=group.ctrldisterror;
tmpmemb=group.nummembers;
tmpsprd=group.distspread;
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_total=nanmean(tmpmean,2);
ctrldist_total=nanmean(tmpctrl,2);
distsprd_total=nanmean(tmpsprd,2);

%Para el hemisferio sur
index=group.shemi==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemi=nanmean(tmpmean,2);
ctrldist_shemi=nanmean(tmpctrl,2);
distsprd_shemi=nanmean(tmpsprd,2);

%Para el hemisferio norte
index=group.nhemi==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemi=nanmean(tmpmean,2);
ctrldist_nhemi=nanmean(tmpctrl,2);
distsprd_nhemi=nanmean(tmpsprd,2);

%Para la estacion calida
index=group.calida==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb  < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_calida=nanmean(tmpmean,2);
ctrldist_calida=nanmean(tmpctrl,2);
distsprd_calida=nanmean(tmpsprd,2);

%Para la estacion fria
index=group.fria==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_fria=nanmean(tmpmean,2);
ctrldist_fria=nanmean(tmpctrl,2);
distsprd_fria=nanmean(tmpsprd,2);



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

title('Distance error by hemisphere');legend('SH MEAN ERROR','NH MEAN ERROR','SH CTRL ERROR','NH CTRL ERROR');


subplot(1,3,3)
hold on
plot(meandist_calida,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_fria,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_calida,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_fria,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

plot(distsprd_calida,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_fria,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Distance error by season');legend('WARM MEAN ERROR','COLD MEAN ERROR','WARM CTRL ERROR','COLD CTRL ERROR');


%OTRA FIGURA PARA VER LOS ERRORES POR HEMISFERIO Y POR ESTACION (CALIDA O
%FRIA).

%Para el hemisferio sur y estacion calida
index=group.shemi==1 & group.calida==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemic=nanmean(tmpmean,2);
ctrldist_shemic=nanmean(tmpctrl,2);
distsprd_shemic=nanmean(tmpsprd,2);

%Para el hemisferio norte y estacion calida
index=group.nhemi==1 & group.calida == 1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemic=nanmean(tmpmean,2);
ctrldist_nhemic=nanmean(tmpctrl,2);
distsprd_nhemic=nanmean(tmpsprd,2);

%Para el hemisferio sur y estacion fria
index=group.shemi==1 & group.fria ==1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb  < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_shemif=nanmean(tmpmean,2);
ctrldist_shemif=nanmean(tmpctrl,2);
distsprd_shemif=nanmean(tmpsprd,2);

%Para el hemisferio norte y la estacion fria.
index=group.nhemi==1 & group.fria == 1;
tmpmean=group.meandisterror(:,index);
tmpctrl=group.ctrldisterror(:,index);
tmpmemb=group.nummembers(:,index);
tmpsprd=group.distspread(:,index);
mask=( isnan(tmpmean) | isnan(tmpctrl) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.
tmpmean(mask)=NaN;
tmpctrl(mask)=NaN;
tmpsprd(mask)=NaN;
meandist_nhemif=nanmean(tmpmean,2);
ctrldist_nhemif=nanmean(tmpctrl,2);
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

title('Distance error Warm Season');legend('SH WARM MEAN','SH COLD MEAN','SH WARM CTRL','SH WARM CTRL');


subplot(1,2,2)
hold on
plot(meandist_nhemic,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(meandist_nhemif,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(ctrldist_nhemic,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(ctrldist_nhemif,'--','Color',[0.6 0.6 0.6],'LineWidth',2)

plot(distsprd_nhemic,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(distsprd_nhemif,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)

title('Distance error Warm Season');legend('NH WARM MEAN','NH COLD MEAN','NH WARM CTRL','NH WARM CTRL');


clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
save(outfile)


