clear all
ndim=40;
alpha=0.8;
npass=1;

a=zeros(ndim,1);
b=zeros(ndim,1);
c=zeros(ndim,1);
a(:)=0;
a(20)=a(20)+1;
a(3)=a(3)-1.5;
%a(1)=a(1)+1;


for n=1:npass

b(1)=a(1);
for i=2:ndim
  b(i) = alpha*b(i-1) + (1-alpha)*a(i);
end
b(1) = alpha*b(ndim) + (1-alpha)*a(1);
for i=2:ndim
  b(i) = alpha*b(i-1) + (1-alpha)*a(i);
end

c(ndim)=b(ndim);
for i=ndim-1:-1:1
  c(i) = alpha*c(i+1) + (1-alpha)*b(i);
end
c(ndim) = alpha*c(1) + (1-alpha)*b(ndim);
for i=ndim-1:-1:1
  c(i) = alpha*c(i+1) + (1-alpha)*b(i);
end

a=c;

end

a = a / a(20);

figure
plot(a,'Color','blue')
axis([1 40 min(a)-0.1 max(a)+0.1]);
xlabel('GRID POINTS')
