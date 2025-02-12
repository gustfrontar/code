function [xens]=run_ensemble_forecast(xens,par,h2,bst)

[dim , ens_size]=size(xens);


%Integrate the ensemble from the analysis
    for iens=1:ens_size
        x=xens(:,iens)';
        for j=1:bst
            xout=stepit('L63eqs',x,par,h2,dim);
            x=xout;    
        end
        xens(:,iens)=x';
    end
    
    
end