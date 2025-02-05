
function [MeanDir]=MeanDir(Dir,W)

   Sin=sin( Dir*pi/180 );
   Cos=cos( Dir*pi/180 );
   
 
   MeanSin=nansum(W.*Sin)/nansum(W);
   MeanCos=nansum(W.*Cos)/nansum(W);
   
   MeanDir=atan2( MeanSin , MeanCos )*180/pi;       
   
end