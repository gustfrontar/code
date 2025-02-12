%funcion q me genera los graficos de linea para todos los pronosticos

function [ ] = graflinea_fun(variable,nombre,titulo,nivel)

%**************************************************************************
%Genera la fecha q va a aparecer en eje x del grafico
load rmset.mat
p=fecha_archivo';
a=size(p);
aa=a(1);
b=a(1)-2;
bb=a(1)-6;
c=a(1)-10;
cc=a(1)-14
d=a(1)-18;
dd=a(1)-22;
e=a(1)-26;
ee=a(1)-30;
f=p(aa,:);
ff=p(b,:);
g=p(bb,:);
gg=p(c,:);
h=p(cc,:);
hh=p(d,:);
i=p(dd,:);
ii=p(e,:);
j=p(ee,:);
jj=[f 0 0];
k=[ff 0 0];
kk=[g 0 0];
l=[gg 0 0];
ll=[h 0 0];
m=[hh 0 0];
mm=[i 0 0];
n=[ii 0 0];
nn=[j 0 0];
temptreinta=datestr(nn, 'dd/mm'); %genera la fecha de hoy menos 15 dias
temptr=datestr(nn, ' mmm.dd HH '); 
tempveintiseis=datestr(n, 'dd/mm'); %genera la fecha de hoy menos 13 dias
tempveintidos=datestr(mm, 'dd/mm'); %genera la fecha de hoy menos 11 dias
tempdieciocho=datestr(m, 'dd/mm'); %genera la fecha de hoy menos 9 dias
tempcatorce=datestr(ll, 'dd/mm'); %genera la fecha de hoy menos 7 dias
tempdiez=datestr(l, 'dd/mm'); %genera la fecha de hoy menos 5 dias
tempseis=datestr(kk, 'dd/mm'); %genera la fecha de hoy menos 3 dias
tempdos=datestr(k, 'dd/mm'); %genera la fecha de hoy menos 1 dia
tempuno=datestr(jj, 'dd/mm'); %genera la fecha de hoy 
tempu=datestr(jj, ' mmm.dd HH '); 
%*************************************GRAFICOS DE LINEAS*******************
figure
hold on
plot(squeeze(variable(3,:)),'r','DisplayName','Prono24','LineWidth',2)
plot(squeeze(variable(5,:)),'c','DisplayName','Prono48','LineWidth',2)
plot(squeeze(variable(7,:)),'y','DisplayName','Prono72','LineWidth',2)
title(titulo,'FontSize',13)
absisas=strcat('Dias desde ', temptr,'Z ', ' a ', tempu,'Z')
xlabel(absisas,'FontSize',12)
ylabel(nombre,'FontSize',12)
set(gca,'XTick',[0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30]);
set(gca,'XTickLabel',{temptreinta,'',tempveintiseis,'',tempveintidos,'',tempdieciocho,'',tempcatorce,'',tempdiez,'',tempseis,'','',tempuno})
set(gca,'YGrid','on','XGrid','on')

legend({'Prono24','Prono48','Prono72'})
archivo=strcat('linea',nombre,nivel);
print('-depsc',archivo)
close 1;
