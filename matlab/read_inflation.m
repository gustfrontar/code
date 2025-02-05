clear all


datapath='./';
nx=137;
ny=109;

nv3d=9;
nv2d=4;
np2d=0;

nlevs=40;

ntotal=nv3d*nlevs+nv2d+np2d;

inflation=zeros(nx,ny,ntotal);

 file=[datapath 'infl_mul.grd'];
 nfile=fopen(file,'r','b');
 
 for i=1:ntotal
  inflation(:,:,i)=fread(nfile,[nx ny],'single');
 end
 
 fclose(nfile)

