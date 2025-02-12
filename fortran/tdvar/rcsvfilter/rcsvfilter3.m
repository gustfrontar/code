%4th order rf
clear all

ndim = 100;
npass = 1;
za = zeros(ndim,1);
zb = zeros(ndim,1);
zc = zeros(ndim,1);
za(ndim/2) = 1.0;
%za(20) = -.5;

xi = 15.0;
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

p=c-b^2/3;
q=d-b*c/3+2*b^3/27;

t1=0.5*(-q+sqrt(q^2+4*p^3/27));
t2=0.5*(-q-sqrt(q^2+4*p^3/27));

u=t1^(1/3);
v=t2^(1/3);
w=exp(2*pi*1i/3);

k1=u+v-b/3;
k2=u*w+v*w^2-b/3;
k3=u*w^2+v*w-b/3;

%b
%k1+k2+k3
%c
%k1*k2+k2*k3+k3*k1
%d
%k1*k2*k3

k1 = 1/k1;
k2 = 1/k2;
k3 = 1/k3;

z1a=1-k1/2+sqrt((1-k1/2)^2-1);
z2a=1-k2/2+sqrt((1-k2/2)^2-1);
z3a=1-k3/2+sqrt((1-k3/2)^2-1);

z1b=1-k1/2-sqrt((1-k1/2)^2-1);
z2b=1-k2/2-sqrt((1-k2/2)^2-1);
z3b=1-k3/2-sqrt((1-k3/2)^2-1);
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

a1 = real(z1+z2+z3)
a2 = real(-(z1*z2+z2*z3+z3*z1))
a3 = real(z1*z2*z3)

a1+a2+a3

zamax = max(za);

zb = za/zamax;

for n=1:npass
for i=4:ndim-3
  zb(i) = (1-a1-a2-a3)*za(i) ...
        + a1*zb(i-1) + a2*zb(i-2) + a3*zb(i-3);
end
end

zc = zb;
for n=1:npass
for i=ndim-3:-1:4
  zc(i) = (1-a1-a2-a3)*zb(i) ...
        + a1*zc(i+1) + a2*zc(i+2) + a3*zc(i+3);
end
end

zcmax = max(zc);
zc = zc / zcmax * zamax;

plot(zc)
