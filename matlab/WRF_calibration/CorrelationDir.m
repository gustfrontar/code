
function [ Correlation ]=CorrelationDir(Dir1,Dir2)

   Sin1=sin( Dir1*pi/180 );
   Cos1=cos( Dir1*pi/180 );
   
   Sin2=sin( Dir2*pi/180 );
   Cos2=cos( Dir2*pi/180 );
   
   tmp1=corrcoef(Sin1,Sin2);
   tmp2=corrcoef(Cos1,Cos2);
   
   Correlation=( tmp1(1,2) + tmp2(1,2) )/2;
 
end