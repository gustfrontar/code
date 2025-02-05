function [M]=SmoothMatrix(M,srow,scol)

%Usamos las funciones para suavizar series temporales para suavizar una
%matriz.

[NRow NCol]=size(M);

for ir=1:NRow
   
    M(ir,:)=smooth(M(ir,:),srow);
    
    
end

for ic=1:NCol
    
   M(:,ic)=smooth(M(:,ic),scol); 
    
    
end


