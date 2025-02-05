
function [ ] = grafbar_fun(variable,nombre,nivel, valores, precision)

%Genera la fecha q va  aparecer en en el titulo
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
tempveintiseis=datestr(n, 'dd/mm'); %genera la fecha de hoy menos 13 dias
tempveintidos=datestr(mm, 'dd/mm'); %genera la fecha de hoy menos 11 dias
tempdieciocho=datestr(m, 'dd/mm'); %genera la fecha de hoy menos 9 dias
tempcatorce=datestr(ll, 'dd/mm'); %genera la fecha de hoy menos 7 dias
tempdiez=datestr(l, 'dd/mm'); %genera la fecha de hoy menos 5 dias
tempseis=datestr(kk, 'dd/mm'); %genera la fecha de hoy menos 3 dias
tempdos=datestr(k, 'dd/mm'); %genera la fecha de hoy menos 1 dia
tempuno=datestr(jj, 'dd/mm'); %genera la fecha de hoy 
%*************************************GRAFICOS DE BARRAS*******************
figure
bar(squeeze(variable(2:7,:)),'EdgeColor','none')
titulo=strcat('Desde ',temptreinta, ' hasta  ', tempuno)
title(titulo,'FontSize',13)
xlabel('Pronosticos','FontSize',12)
ylabel(nombre,'FontSize',12)
set(gca,'XTick',[1 2 3 4 5 6])
set(gca,'XTickLabel',{'12';'24';'36';'48';'60';'72'})
set(gca,'YGrid','on','XGrid','on')

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
colorbar ('YTick',valores,'YTickLabel',{temptreinta,'',tempveintiseis,'',tempveintidos,'',tempdieciocho,'',tempcatorce,'',tempdiez,'',tempseis,'','',tempuno})
archivo=strcat('barras',nombre,nivel);
print('-depsc',archivo)
close 1;
