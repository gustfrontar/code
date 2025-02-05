
function [MeanErrorVel StdErrorVel MeanErrorDir StdErrorDir ]=StatisticsVelDirError(VelFor,DirFor,VelObs,DirObs)

%El error en velocidad, su media y su desvio lo calculamos directamente.

VelError=VelFor-VelObs;

MeanErrorVel=nanmean(VelError);

StdErrorVel=nanstd(VelError);

%La desviacion estandard en direccion la estimo a partir de la muestra de
%direcciones. Pero primero 

DirError=DirFor-DirObs;
  
    for ii=1:length(DirError)
       
        if( DirError(ii) > 180 )
            
            DirError(ii)=DirError(ii)-360;
            
        elseif( DirError(ii) < -180)
            
            DirError(ii)=360+DirError(ii);
            
        end
        
    end
    
    
 %plot(DirError)   
    
 MeanErrorDir=nanmean(DirError);
 
 StdErrorDir=nanstd(DirError);
  
end