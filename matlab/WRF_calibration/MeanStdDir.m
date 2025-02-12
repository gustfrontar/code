
function [MeanDir StdDir]=MeanStdDir(Dir,W)

   Vel=ones(size(Dir));
   [U,V]=VelDirToUV(Vel,Dir);
   
   MeanU=nansum(W.*U)/nansum(W);
   MeanV=nansum(W.*V)/nansum(W);
   
   [Null MeanDir]=UVToVelDir(MeanU,MeanV);
   
   Diff=Dir-MeanDir;
   for ii=1:length(Diff)
       if( Diff > 180);Diff(ii)=Diff(ii)-360;end
       if( Diff < -180);Dir(ii)=Diff(ii)+360;end
   end
   
   StdDir=sqrt( nanmean(W.*(Diff).^2)/nanmean(W) );
   
   
   %plot(Diff)
   
end