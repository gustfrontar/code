function [order_var , order_azimuth , levels]= order_variable(var,azimuth,elevation)

levels=unique(elevation);

azangles=round(length(azimuth)/length(levels));

order_var=NaN(azangles,size(var,1),length(levels));
order_azimuth=NaN(azangles,length(levels));

for ilev=1:length(levels)

    tmp= elevation==levels(ilev); 
    
    tmpvar=var(:,tmp);
    
    [order_azimuth(:,ilev) , sort_index]=sort(azimuth(tmp));

    %size(tmpvar)
    %size(order_var)
    %size(tmpvar(:,sort_index)')
    order_var(:,:,ilev)=tmpvar(:,sort_index)';
    
    
end













end
