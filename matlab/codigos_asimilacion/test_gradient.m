clear all
close all

bst=8;
dt=0.01;
ntimes=4;

x0b=[5.318217030478297  -1.382912086671714  31.364914753651490];


%dx=10*[-1:0.05:1];
%dy=10*[-1:0.05:1];

H=eye(3);

B0=1*[ 0.074385899676410   0.104534234246158   0.001748332887307;...
       0.104534234246158   0.199828677625227  -0.002132888082558;...
       0.001748332887307  -0.002132888082558   0.166967482867106];
%B0=eye(3);

R=eye(3);

invR=inv(R);
invB0=inv(B0);



xt0=x0b+[0.2 0.2 0.2]; 
x=xt0;
y(1,:)=H*x';
for ii=1:ntimes
   for it=1:bst
       x=stepit(x,dt);
   end
   y(ii+1,:)=H*x'; 
end



dx=0.001;
dy=0.001;
dz=0.001;


[J nablaJ]=j_and_gradj(x,x0b,y,H,invR,invB0,dt,bst,ntimes);


xprima=[x(1)+dx x(2) x(3)];
[J1]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);
xprima=[x(1)-dx x(2) x(3)];
[J2]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);

nablaJprima(1)=(J1-J2)/(2*dx);

xprima=[x(1) x(2)+dy x(3)];
[J1]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);
xprima=[x(1) x(2)-dy x(3)];
[J2]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);
nablaJprima(2)=(J1-J2)/(2*dy);

xprima=[x(1) x(2) x(3)+dz];
[J1]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);
xprima=[x(1) x(2) x(3)-dz];
[J2]=j_and_gradj(xprima,x0b,y,H,invR,invB0,dt,bst,ntimes);
nablaJprima(3)=(J1-J2)/(2*dz);


nablaJprima
nablaJ'
