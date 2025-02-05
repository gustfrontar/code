clear all
ndim=40;
xi=zeros(ndim,1);
for i=1:ndim
  xi(i)=sin(2*3.1415926535*i/ndim)+2;
end
xi = xi * 3;
xi(:) = 6;
figure
plot(xi)
axis([1 40 2 10]);
xlabel('GRID POINTS')
ylabel('CORR LENGTH SCALE')

figure
hold on

for npass=4:4

a=zeros(ndim,1);
b=zeros(ndim,1);
c=zeros(ndim,1);
ze=zeros(ndim,1);
alpha=zeros(ndim,1);
sfact=zeros(ndim,1);
a(:)=0;
a(20)=a(20)+1;
original=a;
%a(1)=a(1)+1;

for i=1:ndim
  ze(i) = npass/xi(i)^2;
  alpha(i)=1+ze(i)-sqrt(ze(i)*(ze(i)+2));
  sfact(i)=sqrt(2*3.1415926535)*xi(i)*0.9;
end
alpha(20)

for n=1:npass

b(1)=a(1);
for i=2:ndim
  b(i) = alpha(i)*b(i-1) + (1-alpha(i))*a(i);
end
b(1) = alpha(1)*b(ndim) + (1-alpha(1))*a(1);
for i=2:ndim
  b(i) = alpha(i)*b(i-1) + (1-alpha(i))*a(i);
end

c(ndim)=b(ndim);
for i=ndim-1:-1:1
  c(i) = alpha(i)*c(i+1) + (1-alpha(i))*b(i);
end
c(ndim) = alpha(ndim)*c(1) + (1-alpha(ndim))*b(ndim);
for i=ndim-1:-1:1
  c(i) = alpha(ndim)*c(i+1) + (1-alpha(ndim))*b(i);
end

a=c;

end
%a = a.*sfact
a = a*sfact(10);
a(20)

if (npass==1)
  plot(a,'Color','blue')
elseif (npass==2)
  plot(a,'Color','green')
elseif (npass==3)
  plot(a,'Color','yellow')
elseif (npass==4)
  plot(a,'Color','red')
elseif (npass==5)
  plot(a,'Color','black')
elseif (npass==6)
  plot(a,'Color','green')
end
hold on
plot(original,'Color','Magenta')
axis([1 40 0 1.5]);
xlabel('GRID POINTS')

end
