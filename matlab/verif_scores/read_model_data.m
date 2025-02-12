clear all
close all


%Get files

tmp=dir('../data/*.dat');

nfiles=size(tmp,1);

ntimes=49;

nx_wrf=399;
ny_wrf=399;

for ii=1:nfiles

 filename=['../data/' tmp(ii).name];

 nfile=fopen(filename,'r','b');

  for tt=1:ntimes
   tmpdata(:,:,tt)=fread(nfile,[nx_wrf ny_wrf],'single')';
  end

 fclose(nfile)

  jj=4;
  index=1;
  while ( jj <= ntimes )
   lluvia_modelo(:,:,index,ii)=tmpdata(:,:,jj)-tmpdata(:,:,jj-3);

   jj=jj+3;
   index=index+1;
  end

end





save('model_data.mat','lluvia_modelo')
