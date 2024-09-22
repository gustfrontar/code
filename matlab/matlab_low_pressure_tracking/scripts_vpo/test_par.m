clear all
close all

matlabpool

A=NaN(1,2000000);
tic
for i=1:2000000
    A(i)=i;
end
toc

tic
parfor i=1:2000000
    A(i)=i;
end
toc

matlabpool close