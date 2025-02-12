
function [bin]=write_file(open_file,nx,ny,nz,data,cond)


% open file tiene que venir de afuera (string)
% nx ny y nz son las dimensiones de la matriz y tienen que venir de afuera
% data es la matriz que viene de afuera



if (cond==1)
    bin = fopen(open_file,'w');
    for j=1:nz
    fwrite(bin,squeeze(data(:,:,j))','float32',0,'ieee-le');
    end
    fclose(bin);
else 
    bin=fopen(open_file,'r');
    for i=1:nz
    var(:,:,i)=fread(bin,[ny nx],'float32','ieee-le')';
    end
    fclose(bin);
end
                                                                                                                             
                                                                                                                             

