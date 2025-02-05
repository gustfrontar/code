clear all
close all
fclose all

%datapath='/data/letkf02/jruiz/EXP_40mem_sfflux_ntpar_nsmooth_fixparinf_qfxust/anal/200809160600/';
%datapath='/data/letkf02/jruiz/TEST/anal/200808061800/';
datapath='./';
nx=137;
ny=109;

nv3d=9;
nv2d=4;
np2d=3;

nlevs=40;
nmembers=40;

ntotal=nv3d*nlevs+nv2d+np2d;

varg=zeros(nx,ny,ntotal,nmembers);
vara=zeros(nx,ny,ntotal,nmembers);

  for member=1:nmembers
      member
  if(member < 10)
  file=[datapath 'gs0400' num2str(member) '.grd'];
  else
  file=[datapath 'gs040' num2str(member) '.grd'];
  end
  
  nfile=fopen(file,'r','b');
  file 
  for i=1:ntotal
   varg(:,:,i,member)=fread(nfile,[nx ny],'single');
  end
 
  fclose(nfile) 
  end

  for member=1:nmembers
      member
  if(member < 10)
  file=[datapath 'gs0400' num2str(member) '.grd'];
  else
  file=[datapath 'gs040' num2str(member) '.grd'];
  end

  nfile=fopen(file,'w','b');
  varg(:,:,365:367,member)=0.25*randn*ones(size(varg,1),size(varg,2),3);

  for i=1:ntotal
   fwrite(nfile,squeeze(varg(:,:,i,member)),'single');
  end

  fclose(nfile)
  end

