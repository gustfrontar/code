%4th order rf
clear all

ndim = 100;
za = zeros(ndim,1);
zb = zeros(ndim,1);
zc = zeros(ndim,1);
za(ndim/2) = 1.0;
%za(2) = -1.5;

xi = 10.0;
figure
hold on

for i=1:ndim
  zc(i) = exp(-(i-ndim/2)^2/xi^2/2);
end 
plot(zc,'Color','green')

b=xi^2/2;
c=xi^2/24+xi^4/8;
%c=xi^4/8;
d=xi^2/180+xi^4/48+xi^6/48;
%d=xi^6/48;
e=xi^2/1120+xi^4*7/1920+xi^6/192+xi^8/384;
%e=xi^8/384;

p=c-3/8*b^2;
q=d-b*c/2+b^3/8;
r=e-b*d/4+b^2*c/16-3*b^4/256;

b2=p/2;
c2=p^2/16-r/4;
d2=-q^2/64;

p2=c2-b2^2/3;
q2=d2-b2*c2/3+2*b2^3/27;

t1=0.5*(-q2+sqrt(q2^2+4*p2^3/27));
t2=0.5*(-q2-sqrt(q2^2+4*p2^3/27));

u=t1^(1/3);
v=t2^(1/3);
w=exp(2*pi*1i/3);

t1=u+v-b2/3;
t2=u*w+v*w^2-b2/3;
t3=u*w^2+v*w-b2/3;

u=sqrt(t1);
v=sqrt(t2);
w=sqrt(t3);

w=w*real(-u*v*w*8/q);

k1=u+v+w-b/4;
k2=u-v-w-b/4;
k3=-u+v-w-b/4;
k4=-u-v+w-b/4;

%b
%k1+k2+k3+k4
%c
%k1*k2+k1*k3+k1*k4+k2*k3+k2*k4+k3*k4
%d
%k1*k2*k3+k2*k3*k4+k3*k4*k1+k4*k1*k2
%e
%k1*k2*k3*k4

k1 = 1/k1;
k2 = 1/k2;
k3 = 1/k3;
k4 = 1/k4;

z1a=1-k1/2+sqrt((1-k1/2)^2-1);
z2a=1-k2/2+sqrt((1-k2/2)^2-1);
z3a=1-k3/2+sqrt((1-k3/2)^2-1);
z4a=1-k4/2+sqrt((1-k4/2)^2-1);

z1b=1-k1/2-sqrt((1-k1/2)^2-1);
z2b=1-k2/2-sqrt((1-k2/2)^2-1);
z3b=1-k3/2-sqrt((1-k3/2)^2-1);
z4b=1-k4/2-sqrt((1-k4/2)^2-1);
if(abs(z1a)>abs(z1b))
  z1 = z1b;
else
  z1 = z1a;
end
if(abs(z2a)>abs(z2b))
  z2 = z2b;
else
  z2 = z2a;
end
if(abs(z3a)>abs(z3b))
  z3 = z3b;
else
  z3 = z3a;
end
if(abs(z4a)>abs(z4b))
  z4 = z4b;
else
  z4 = z4a;
end

a1 = real(z1+z2+z3+z4)
a2 = real(-(z1*z2+z1*z3+z1*z4+z2*z3+z2*z4+z3*z4))
a3 = real(z1*z2*z3+z2*z3*z4+z3*z4*z1+z4*z1*z2)
a4 = real(-z1*z2*z3*z4)

a1+a2+a3+a4

zamax = max(abs(za));
za = za/zamax;

zb(:) = 0;
for i=5:ndim
  zb(i) = (1-a1-a2-a3-a4)*za(i) ...
        + a1*zb(i-1) + a2*zb(i-2) + a3*zb(i-3) + a4*zb(i-4);
end
zb(1) = (1-a1-a2-a3-a4)*za(1) ...
      + a1*zb(ndim) + a2*zb(ndim-1) + a3*zb(ndim-2) + a4*zb(ndim-3);
zb(2) = (1-a1-a2-a3-a4)*za(2) ...
      + a1*zb(1) + a2*zb(ndim) + a3*zb(ndim-1) + a4*zb(ndim-2);
zb(3) = (1-a1-a2-a3-a4)*za(3) ...
      + a1*zb(2) + a2*zb(1) + a3*zb(ndim) + a4*zb(ndim-1);
zb(4) = (1-a1-a2-a3-a4)*za(4) ...
      + a1*zb(3) + a2*zb(2) + a3*zb(1) + a4*zb(ndim);
for i=5:ndim
  zb(i) = (1-a1-a2-a3-a4)*za(i) ...
        + a1*zb(i-1) + a2*zb(i-2) + a3*zb(i-3) + a4*zb(i-4);
end

zc(:) = 0;
for i=ndim-4:-1:1
  zc(i) = (1-a1-a2-a3-a4)*zb(i) ...
        + a1*zc(i+1) + a2*zc(i+2) + a3*zc(i+3) + a4*zc(i+4);
end
zc(ndim) = (1-a1-a2-a3-a4)*zb(ndim) ...
      + a1*zc(1) + a2*zc(2) + a3*zc(3) + a4*zc(4);
zc(ndim-1) = (1-a1-a2-a3-a4)*zb(ndim-1) ...
      + a1*zc(ndim) + a2*zc(1) + a3*zc(2) + a4*zc(3);
zc(ndim-2) = (1-a1-a2-a3-a4)*zb(ndim-2) ...
      + a1*zc(ndim-1) + a2*zc(ndim) + a3*zc(1) + a4*zc(2);
zc(ndim-3) = (1-a1-a2-a3-a4)*zb(ndim-3) ...
      + a1*zc(ndim-2) + a2*zc(ndim-1) + a3*zc(ndim) + a4*zc(1);
for i=ndim-4:-1:1
  zc(i) = (1-a1-a2-a3-a4)*zb(i) ...
        + a1*zc(i+1) + a2*zc(i+2) + a3*zc(i+3) + a4*zc(i+4);
end

zcmax = max(abs(zc));
zc = zc / zcmax * zamax;

plot(zc)
