clear all
close all

bst=8;
dt=0.01;
ntimes=7;

x0b=[5.318217030478297  -1.382912086671714  31.364914753651490];

dx=10*[-1:0.05:1];
dy=10*[-1:0.05:1];

H=eye(3);

B0=1*[ 0.074385899676410   0.104534234246158   0.001748332887307;...
       0.104534234246158   0.199828677625227  -0.002132888082558;...
       0.001748332887307  -0.002132888082558   0.166967482867106];
%B0=eye(3);

R=eye(3);

invR=inv(R);
invB0=inv(B0);

xt0=x0b+randn(1,3); 
x=xt0;
y(1,:)=H*x';
for ii=1:ntimes
   for it=1:bst
       x=stepit(x,dt);
   end
   y(ii+1,:)=H*x'+randn(3,1); 
end

for ii=1:length(dx)
    for jj=1:length(dy)

      xprima=[x0b(1)+dx(ii) x0b(2)+dy(jj) x0b(3)] ; 
      [J(jj,ii) nablaJ(:,jj,ii)]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);

    end
end

%CHEQUEAR MODELO TANGENTE Y EL ADJUNTO.

options = optimset('GradObj','on','TolX',1e-15);
[minX,minJ,nada,info,nablaminJ] = fminunc('j_and_gradj',x0b,options,x0b,y,H,invR,invB0,dt,bst,ntimes);
options = optimset();
[minX2,minJ2,nada,info2,nablaminJ2] = fminunc('j_and_gradj',x0b,options,x0b,y,H,invR,invB0,dt,bst,ntimes);

pcolor(x0b(1)+dx,x0b(2)+dy,J)
shading flat
hold on
quiver(x0b(1)+dx,x0b(2)+dy,squeeze(nablaJ(1,:,:)),squeeze(nablaJ(2,:,:)),'k')
plot(minX(1),minX(2),'ko','MarkerSize',12)
plot(xt0(1),xt0(2),'ro','MarkerSize',12)
plot(x0b(1),x0b(2),'go','MarkerSize',12)
