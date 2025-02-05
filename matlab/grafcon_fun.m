%funcion q me genera los graficos de contornos para cada pronosticos sobre
%todos los niveles

function [ ] = grafcon_fun(variable,nombre,titulo,tiempo,valores,precision)

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
%**************************************************************************
%*************************************GRAFICOS DE CONTORNOS*******************
figure
[cs h]=contourf(squeeze(variable),'LevelList',valores);
title(titulo,'FontSize',13)
absisas=strcat('Dias desde ', temptr,'Z ', ' a ', tempu,'Z')
xlabel(absisas,'FontSize',12)
ylabel('Niveles de Presion','FontSize',12)
set(gca,'YTick',[1 2 3 4 5 6]);
set(gca,'YTickLabel',{'900';'850';'700';'500';'300';'200'})
set(gca,'XTick',[0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30]);
set(gca,'XTickLabel',{temptreinta,'',tempveintiseis,'',tempveintidos,'',tempdieciocho,'',tempcatorce,'',tempdiez,'',tempseis,'','',tempuno})

mima=[min(valores) max(valores)];
caxis(mima);
%hcs=clabel(cs,h,'LabelSpacing',720,'FontSize',12,'Fontweight','bold');
prec=precision;
step=(mima(2)-mima(1))*prec;
hh=colormap(jet(step));
Vv=[valores(1):1/prec:valores(end)]; Vv=round(Vv*10e5)/10e5;
Vv=Vv(1:end-1)';
clear pos
for i=1:size(valores,2)-1
    pos(i)=find(Vv==valores(i));
end
pos=[pos length(Vv)];  %pos contains the position of V values in Vv vector.
% it means that pos contains the position of V values also
% in hh color vector!
hhh=colormap(jet(length(pos)));
for i=1:length(pos)-1
hh(pos(i):pos(i+1)-1,:)=repmat( hhh(i,:),size(hh(pos(i):pos(i+1)-1,:),1),1);
end
hh(end,:)=hh(end-1,:);
colormap(hh);
colorbar;
colorbar('YTick',valores);
archivo=strcat('cont',nombre,tiempo);
print('-depsc',archivo)
close 1;