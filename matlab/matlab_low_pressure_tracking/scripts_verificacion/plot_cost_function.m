clear all
close all
addpath('../common_functions_vpo/');

%ESTE SCRIPT TOMA LA FUNCION DE COSTO UTILIZADA POR EL ALGORITMO DE
%TRACKING Y GRAFICA SUS VALORES EN DIFERENTES SITUACIONES, PARA ANALIZAR SU
%COMPORTAMIENTO TEORICO Y VERIFICAR QUE TODO ESTE FUNCIONANDO
%CORRECTAMENTE.


VMAX=30;            %VELOCIDAD MAXIMA.
DXMAX=30*3600*6;    %DESPLAZAMIENTO MAXIMO.



LON1=0;
LON2=1;

LAT1=0;
LAT2=0;


RESOLUCION=0.5;
DOMX=30;
DOMY=30;

DELTALAT=-DOMY/2:RESOLUCION:DOMY/2;
DELTALON=-DOMX/2:RESOLUCION:DOMY/2;

count=0;
figure
for LON2=0:2:6


for ii=1:length(DELTALON)
    
    for jj=1:length(DELTALAT)
       
        LAT3(ii,jj)=LAT2+DELTALAT(jj);
        LON3(ii,jj)=LON2+DELTALON(ii);
        
        J(ii,jj)= test_cost_function_trajectory(LON1,LAT1,LON2,LAT2,LON3(ii,jj),LAT3(ii,jj),1,1,DXMAX,1);
               
        
    end
    
end

tmplon=-DXMAX/111000:1:DXMAX/111000;
tmplat1=sqrt((DXMAX/111000).^2-tmplon.^2);
tmplat2=-sqrt((DXMAX/111000).^2-tmplon.^2);

count=count+1;
subplot(2,2,count)

hold on
pcolor(LON3,LAT3,J);
shading flat
plot(LON1,LAT1,'ko','MarkerSize',15);
plot(LON2,LAT2,'ko','MarkerSize',15);
contour(LON3,LAT3,J,[0.1 1000],'LineColor','k','LineWidth',2)
plot(tmplon+LON2,tmplat1,'kv','LineWidth',2);
plot(tmplon+LON2,tmplat2,'kv','LineWidth',2);

title(['LON1=0, LON2=' num2str(LON2) ' COST FUNCTION 1']);

end


count=0;
figure
for LON2=0:2:6


for ii=1:length(DELTALON)
    
    for jj=1:length(DELTALAT)
       
        LAT3(ii,jj)=LAT2+DELTALAT(jj);
        LON3(ii,jj)=LON2+DELTALON(ii);
        
        J(ii,jj)= test_cost_function_trajectory(LON1,LAT1,LON2,LAT2,LON3(ii,jj),LAT3(ii,jj),1,1,DXMAX,2);
               
        
    end
    
end

tmplon=-DXMAX/111000:1:DXMAX/111000;
tmplat1=sqrt((DXMAX/111000).^2-tmplon.^2);
tmplat2=-sqrt((DXMAX/111000).^2-tmplon.^2);

count=count+1;
subplot(2,2,count)

hold on
pcolor(LON3,LAT3,J);
shading flat
plot(LON1,LAT1,'ko','MarkerSize',15);
plot(LON2,LAT2,'ko','MarkerSize',15);
contour(LON3,LAT3,J,[0.5 1000],'LineColor','k','LineWidth',2)
plot(tmplon+LON2,tmplat1,'kv','LineWidth',2);
plot(tmplon+LON2,tmplat2,'kv','LineWidth',2);

title(['LON1=0, LON2=' num2str(LON2) ' COST FUNCTION 2']);

end



count=0;
figure
for LON2=0:2:6


for ii=1:length(DELTALON)
    
    for jj=1:length(DELTALAT)
       
        LAT3(ii,jj)=LAT2+DELTALAT(jj);
        LON3(ii,jj)=LON2+DELTALON(ii);
        
        J(ii,jj)= test_cost_function_trajectory(LON1,LAT1,LON2,LAT2,LON3(ii,jj),LAT3(ii,jj),1,1,DXMAX,3);
               
        
    end
    
end

tmplon=-DXMAX/111000:1:DXMAX/111000;
tmplat1=sqrt((DXMAX/111000).^2-tmplon.^2);
tmplat2=-sqrt((DXMAX/111000).^2-tmplon.^2);

count=count+1;
subplot(2,2,count)

hold on
pcolor(LON3,LAT3,J);
shading flat
plot(LON1,LAT1,'ko','MarkerSize',15);
plot(LON2,LAT2,'ko','MarkerSize',15);
contour(LON3,LAT3,J,[0.5 1000],'LineColor','k','LineWidth',2)
plot(tmplon+LON2,tmplat1,'kv','LineWidth',2);
plot(tmplon+LON2,tmplat2,'kv','LineWidth',2);

title(['LON1=0, LON2=' num2str(LON2) ' COST FUNCTION 3']);

end

