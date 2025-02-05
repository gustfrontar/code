function []=Plot_Tri_function(var,tit) 

figure
    for idir=1:4;
        subplot(2,2,idir)
            plot(var(:,idir))
            hold on
            title(tit)
            axis([0 length(var) min(var(:,idir)) max(var(:,idir))]);   
    end
return
    
    