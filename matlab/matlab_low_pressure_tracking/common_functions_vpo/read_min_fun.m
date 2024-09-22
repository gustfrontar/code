function [MinStruct]=read_min_fun(InputFile)

%This function reads the output from the program findminimun.f90 and orders
%into a matlab structure.

fid=fopen(InputFile);
if(fid < 0)
    fprintf('ERROR: Archivo no encontrado!!')
    MinStruct=NaN;
    return
end
%Read the dimensions of the matrices used for the minimun and systems
%search.
fread(fid,1,'single');
MinStruct.nx=fread(fid,1,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.ny=fread(fid,1,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.nf=fread(fid,1,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.nminimos=fread(fid,1,'single');
fread(fid,1,'single');

%See if some additional data has been procesed (temperature for example).
fread(fid,1,'single');
tmpflag=fread(fid,1,'single');
MinStruct.additionaldataflag=(tmpflag==1);
fread(fid,1,'single');

nminimos=MinStruct.nminimos;
%Read latitudes and longitudes
fread(fid,1,'single');
MinStruct.minlon=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.minlat=fread(fid,nminimos,'single');
fread(fid,1,'single');

fread(fid,1,'single');
MinStruct.minlonint=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.minlatint=fread(fid,nminimos,'single');
fread(fid,1,'single');

fread(fid,1,'single');
MinStruct.minloncent=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.minlatcent=fread(fid,nminimos,'single');
fread(fid,1,'single');

%Read the subindices corresponding to each minimun.
fread(fid,1,'single');
MinStruct.mini=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.minj=fread(fid,nminimos,'single');
fread(fid,1,'single');

%Read min id
fread(fid,1,'single');
MinStruct.minid=fread(fid,nminimos,'single');
fread(fid,1,'single');

%Read min area
fread(fid,1,'single');
MinStruct.minarea=fread(fid,nminimos,'single');
fread(fid,1,'single');

%Read min anom and minimun.
fread(fid,1,'single');
MinStruct.minanomsis=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.meananomsis=fread(fid,nminimos,'single');
fread(fid,1,'single');

%If present read additional data.
for jj=1:MinStruct.nf
if(MinStruct.additionaldataflag)
fread(fid,1,'single');
MinStruct.minanomsis(:,jj)=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.maxanomsis(:,jj)=fread(fid,nminimos,'single');
fread(fid,1,'single');
fread(fid,1,'single');
MinStruct.meananomsis(:,jj)=fread(fid,nminimos,'single');
fread(fid,1,'single');
end
end

fread(fid,1,'single');
MinStruct.mingridsize=fread(fid,nminimos,'single');
fread(fid,1,'single');

for II=1:nminimos
   fread(fid,1,'single');
   MinStruct.minindex{II}=fread(fid,MinStruct.mingridsize(II),'single');
   fread(fid,1,'single');
end

fclose(fid);
