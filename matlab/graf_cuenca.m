clear all
close all



%********************************En puntos de grilla*****************
load pp_promedio_cuenca.mat
load pp_promediocmorph_cuenca.mat

tam=size(promedio,1);
tam_inicial=(tam-31);
tam2=tam-2;
tam6=tam-6;
tam10=tam-10;
tam14=tam-14;
tam18=tam-18;
tam22=tam-22;
tam26=tam-26;
tam30=tam-30;

a=promedio(tam,1);
aa=promedio(tam_inicial,1);
a2=promedio(tam2,1);
a6=promedio(tam6,1);
a10=promedio(tam10,1);
a14=promedio(tam14,1);
a18=promedio(tam18,1);
a22=promedio(tam22,1);
a26=promedio(tam26,1);
a30=promedio(tam30,1);
b=num2str(a);
bb=num2str(aa);
b2=num2str(a2);
b6=num2str(a6);
b10=num2str(a10);
b14=num2str(a14);
b18=num2str(a18);
b22=num2str(a22);
b26=num2str(a26);
b30=num2str(a30);
c=datenum(b,'yyyymmdd');
cc=datenum(bb,'yyyymmdd');
c2=datenum(b2,'yyyymmdd');
c6=datenum(b6,'yyyymmdd');
c10=datenum(b10,'yyyymmdd');
c14=datenum(b14,'yyyymmdd');
c18=datenum(b18,'yyyymmdd');
c22=datenum(b22,'yyyymmdd');
c26=datenum(b26,'yyyymmdd');
c30=datenum(b30,'yyyymmdd');
e=datevec(c);
ee=datevec(cc);
e2=datevec(c2);
e6=datevec(c6);
e10=datevec(c10);
e14=datevec(c14);
e18=datevec(c18);
e22=datevec(c22);
e26=datevec(c26);
e30=datevec(c30);
tiempo_fi=datestr(e, ' dd/mm');
tiempo_in=datestr(ee, ' dd/mm');
tiempo_2=datestr(e2, 'dd/mm');
tiempo_6=datestr(e6, 'dd/mm');
tiempo_10=datestr(e10, 'dd/mm');
tiempo_14=datestr(e14, 'dd/mm');
tiempo_18=datestr(e18, 'dd/mm');
tiempo_22=datestr(e22, 'dd/mm');
tiempo_26=datestr(e26, 'dd/mm');
tiempo_30=datestr(e30, 'dd/mm');

time_fi=datestr(e, ' dd mmm');
time_in=datestr(ee, ' dd mmm');


figure 
plot(promedio(tam_inicial:tam,6),'-r.','MarkerSize',16,'DisplayName','Pronostico a 24hs','LineWidth',2)
hold on 
plot(promedio(tam_inicial:tam,11),'-b.','MarkerSize',16,'DisplayName','Pronostico a 48hs','LineWidth',2)
plot(promedio(tam_inicial:tam,16),'-m.','MarkerSize',16,'DisplayName','Pronostico a 72hs','LineWidth',2)
plot(promedio_c(tam_inicial:tam,6),'-k.','MarkerSize',16,'DisplayName','Observacion Cmorph','LineWidth',2)

absisas=strcat('Desde el ', time_in,' a las 12Z al ', time_fi,' a las 12Z')
title(absisas,'FontSize',13)
xlabel('Dias','FontSize',12)
ylabel('Precipitacion Promedio','FontSize',12)
set(gca,'XTick',[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31]);
set(gca,'XTickLabel',{tiempo_in,'','','',tiempo_26,'','','',tiempo_22,'','','',tiempo_18,'','','',tiempo_14,'','','',tiempo_10,'','','',tiempo_6,'','','',tiempo_2,'',''})
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs','Observacion Cmorph',1)
archivo=strcat('promedio','_cuenca');
print('-depsc',archivo)
close 1


clear all

%***************************En puntos de estaciones***********************

load pp_promediogts_cuenca.mat

tam=size(promedio_e,1);
tam_inicial=tam-31;
tam2=tam-2;
tam6=tam-6;
tam10=tam-10;
tam14=tam-14;
tam18=tam-18;
tam22=tam-22;
tam26=tam-26;
tam30=tam-30;

a=promedio_e(tam,1);
aa=promedio_e(tam_inicial,1);
a2=promedio_e(tam2,1);
a6=promedio_e(tam6,1);
a10=promedio_e(tam10,1);
a14=promedio_e(tam14,1);
a18=promedio_e(tam18,1);
a22=promedio_e(tam22,1);
a26=promedio_e(tam26,1);
a30=promedio_e(tam30,1);
b=num2str(a);
bb=num2str(aa);
b2=num2str(a2);
b6=num2str(a6);
b10=num2str(a10);
b14=num2str(a14);
b18=num2str(a18);
b22=num2str(a22);
b26=num2str(a26);
b30=num2str(a30);
c=datenum(b,'yyyymmdd');
cc=datenum(bb,'yyyymmdd');
c2=datenum(b2,'yyyymmdd');
c6=datenum(b6,'yyyymmdd');
c10=datenum(b10,'yyyymmdd');
c14=datenum(b14,'yyyymmdd');
c18=datenum(b18,'yyyymmdd');
c22=datenum(b22,'yyyymmdd');
c26=datenum(b26,'yyyymmdd');
c30=datenum(b30,'yyyymmdd');
e=datevec(c);
ee=datevec(cc);
e2=datevec(c2);
e6=datevec(c6);
e10=datevec(c10);
e14=datevec(c14);
e18=datevec(c18);
e22=datevec(c22);
e26=datevec(c26);
e30=datevec(c30);
tiempo_fi=datestr(e, ' dd/mm');
tiempo_in=datestr(ee, ' dd/mm');
tiempo_2=datestr(e2, 'dd/mm');
tiempo_6=datestr(e6, 'dd/mm');
tiempo_10=datestr(e10, 'dd/mm');
tiempo_14=datestr(e14, 'dd/mm');
tiempo_18=datestr(e18, 'dd/mm');
tiempo_22=datestr(e22, 'dd/mm');
tiempo_26=datestr(e26, 'dd/mm');
tiempo_30=datestr(e30, 'dd/mm');

time_fi=datestr(e, ' dd mmm');
time_in=datestr(ee, ' dd mmm');




figure 
plot(promedio_e(tam_inicial:tam,3),'-r.','MarkerSize',16,'DisplayName','Pronostico a 24hs','LineWidth',2)
hold on 
plot(promedio_e(tam_inicial:tam,4),'-b.','MarkerSize',16,'DisplayName','Pronostico a 48hs','LineWidth',2)
plot(promedio_e(tam_inicial:tam,5),'-m.','MarkerSize',16,'DisplayName','Pronostico a 72hs','LineWidth',2)
plot(promedio_e(tam_inicial:tam,2),'-k.','MarkerSize',16,'DisplayName','Observacion GTS','LineWidth',2)

absisas=strcat('Desde el ', time_in,' a las 12Z al ', time_fi,' a las 12Z')
title(absisas,'FontSize',13)
xlabel('Dias','FontSize',12)
ylabel('Precipitacion Promedio','FontSize',12)
set(gca,'XTick',[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31]);
set(gca,'XTickLabel',{tiempo_in,'','','',tiempo_26,'','','',tiempo_22,'','','',tiempo_18,'','','',tiempo_14,'','','',tiempo_10,'','','',tiempo_6,'','','',tiempo_2,'',''})
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs','Observacion GTS',1)
archivo=strcat('promedio','_cuenca','_est');
print('-depsc',archivo)
close 1



