%Funcion q grafica y fija la escala de valores con los colores q quiero
%usar, para una escla NO LINEAL
%Se le ingresa latitud, longitud, la variable, la escala de valores y la
%precision q es un valor relativo a la escala de valores
%Va junto con el script de ppcon_fun

function con=contourf2(latitud,longitud,variable,valores,precision)

[cs h]=contourfm(latitud,longitud,variable,valores);
mima=[min(valores) max(valores)];
caxis(mima);
hcs=clabel(cs,h,'LabelSpacing',720,'FontSize',12,'Fontweight','bold'); 
prec=precision;
stepping=(mima(2)-mima(1))*prec;
hh=colormap(jet(stepping));
Vv=[valores(1):1/prec:valores(end)];Vv=Vv(1:end-1)';
clear pos
for i=1:size(valores,2)-1
    pos(i)=find(Vv==valores(i));
end
pos=[pos length(Vv)];               %pos contains the position of V values in Vv vector.
               % it means that pos contains the position of V values also
               % in hh color vector!
hhh=colormap(jet(length(pos)));
for i=1:length(pos)-1
hh(pos(i):pos(i+1)-1,:)=repmat( hhh(i,:),size(hh(pos(i):pos(i+1)-1,:),1),1);
end
hh(end,:)=hh(end-1,:);
colormap(hh);
colorbar;