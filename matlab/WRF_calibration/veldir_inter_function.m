function [inter_vel]=veldir_inter_function(forecast,vel,power,directions)

for i=1:length(forecast)
    if( forecast(2,i)> directions(end) || forecast(2,i) <= directions(1) ); 
        x=vel{1};
        y=power{1};
    elseif( forecast(2,i) > directions(1) && forecast(2,i) <= directions(2) )
        x=vel{2};
        y=power{2};
    elseif( forecast(2,i) > directions(2) && forecast(2,i) <= directions(3) )
        x=vel{3};  
        y=power{3};
    elseif( forecast(2,i) > directions(3) && forecast(2,i) <= directions(4) )
        x=vel{4};  
        y=power{4};           
    elseif( forecast(2,i) > directions(4) && forecast(2,i) <= directions(5) )
        x=vel{5};  
        y=power{5};
    elseif( forecast(2,i) > directions(5) && forecast(2,i) <= directions(6) )
        x=vel{6};  
        y=power{6};
    elseif( forecast(2,i) > directions(6) && forecast(2,i) <= directions(7) )
        x=vel{7};  
        y=power{7};
    elseif( forecast(2,i) > directions(7) && forecast(2,i) <= directions(end) )
        x=vel{8};  
        y=power{8};
    end
    inter_vel(:,i)=interp1(x,y,forecast(1,i),'spline');
end
return