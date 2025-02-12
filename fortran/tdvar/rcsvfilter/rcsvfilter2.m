%2nd order rf
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

a=xi^2/2;
b=xi^2/24+xi^4/8;
%c=xi^4/8;

k1=0.5*(-a+sqrt(a^2-4*b));
k2=0.5*(-a-sqrt(a^2-4*b));

%a
%k1+k2
%b
%k1*k2

k1 = 1/k1;
k2 = 1/k2;

z1a=1-k1/2+sqrt((1-k1/2)^2-1);
z2a=1-k2/2+sqrt((1-k2/2)^2-1);

z1b=1-k1/2-sqrt((1-k1/2)^2-1);
z2b=1-k2/2-sqrt((1-k2/2)^2-1);
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

a1 = real(z1+z2)
a2 = real(-z1*z2)

zamax = max(za);

zb = za/zamax;

for n=1:npass
for i=3:ndim-2
  zb(i) = (1-a1-a2)*za(i) ...
        + a1*zb(i-1) + a2*zb(i-2);
end
end

zc = zb;
for n=1:npass
for i=ndim-2:-1:3
  zc(i) = (1-a1-a2)*zb(i) ...
        + a1*zc(i+1) + a2*zc(i+2);
end
end

zcmax = max(zc);
zc = zc / zcmax * zamax;

plot(zc)
