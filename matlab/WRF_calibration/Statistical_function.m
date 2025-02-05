function [RmseVel,BiasVel,RmseDir,BiasDir,RcoefVel,RcoefDir,PoliVel,PoliDir]=Statistical_function(modelVel,obsVel,modelDir,obsDir)

for i=1:size(modelVel,2)
    RmseVel(i)=sqrt((nansum(modelVel(:,i)-obsVel(:,i))^2)/length(modelVel));
    BiasVel(i)=nansum(modelVel(:,i)-obsVel(:,i))/length(modelVel);
       
    dif=modelDir(:,i)-obsDir(:,i);
    if(abs(dif)>180)
        RmseDir(i)=sqrt((nansum(360-dif)^2)/length(modelDir));
        BiasDir(i)=nansum(360-dif)/length(modelDir);
    else 
        RmseDir(i)=sqrt((nansum(dif)^2)/length(modelDir));
        BiasDir(i)=nansum(dif)/length(modelDir);
    end

    keep_data= ~( isnan(modelVel(:,i)) | isnan(obsVel(:,i)));
    
    mVel=modelVel(keep_data,i);
    oVel=obsVel(keep_data,i);
    
    keep_data= ~( isnan(modelDir(:,i)) | isnan(obsDir(:,i)));
    
    mDir=modelDir(keep_data,i);
    oDir=obsDir(keep_data,i);
    
    [RcoefVel(:,:,i)]=corrcoef(mVel,oVel);
    [RcoefDir(:,:,i)]=corrcoef(mDir,oDir);
    PoliVel(:,2,i)=polyfit(mVel,oVel,1);
    PoliDir(:,2,i)=polyfit(mDir,oDir,1);
end

return