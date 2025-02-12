
function [ U10M , V10M , HR2M , SLP ]=read_arwpost_nature(file,nx,ny) 


  U10M=NaN(ny,nx);
  V10M=NaN(ny,nx);
  HR2M=NaN(ny,nx);
  SLP=NaN(ny,nx);

nfile=fopen(file,'r','b');
if(nfile ~= -1)

         RH2M(:,:)=fread(nfile,[nx ny],'single')';
         RH2M(RH2M>1e15)=NaN;
         U10M(:,:)=fread(nfile,[nx ny],'single')';
         U10M(U10M>1e15)=NaN;
         V10M(:,:)=fread(nfile,[nx ny],'single')';
         V10M(V10M>1e15)=NaN;
         SLP(:,:)=fread(nfile,[nx ny],'single')';
         SLP(SLP>1e15)=NaN;
         
         fclose(nfile);
end


