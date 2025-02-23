clear all
close all

samplesize=10000;
ntimes=10;
bst=8;
dt=0.01;

x=[5.318217030478297  -1.382912086671714  31.364914753651490];

for ii=1:samplesize
ens(ii,:,1)=x+0.1*randn(1,3);

 xf=ens(ii,:,1);
 for jj=1:ntimes
   for k=1:bst
   
       xf=stepit(xf,dt);
   
   end
      
   ens(ii,:,jj+1)=xf;
 end
    
end

%Evolucion de la pdf a distintos tiempos.

figure
hold on
plot(ens(:,1,1)-mean(ens(:,1,1)),ens(:,2,1)-mean(ens(:,2,1)),'go','MarkerSize',1)

plot(ens(:,1,2)-mean(ens(:,1,2)),ens(:,2,2)-mean(ens(:,2,2)),'bo','MarkerSize',1)

plot(ens(:,1,6)-mean(ens(:,1,6)),ens(:,2,6)-mean(ens(:,2,6)),'mo','MarkerSize',1)

plot(ens(:,1,9)-mean(ens(:,1,9)),ens(:,2,9)-mean(ens(:,2,9)),'ro','MarkerSize',1)

axis([-2 2 -2 2])

%Vamos a considerar el ultimo tiempo y a ver como resultaria la hipotesis
%de gausianidad en la estimacion del estado optimo.

covar=cov(ens(:,:,9));

covar=covar(1:2,1:2);





