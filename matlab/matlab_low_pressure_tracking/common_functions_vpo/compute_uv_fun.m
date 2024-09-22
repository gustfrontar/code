function [ u,v,mag ] = compute_uv_fun( lona,lata,lonb,latb,deltat )
%This function computes the velocity of propagating systems from point a to
%point b in a time deltat. It also computes its approximate decomposition
%into a U and V components.

if(deltat==0)
    fprintf('ERROR, DELTAT MUST NOT BE 0')
    return
end

%Compute the mean latitude and mean longitude between points a and b.
mean_lat=0.5*(lata+latb);
mean_lon=0.5*(lona+lonb);
%Compute the displacment in the X and Y direction (using the mean lat and mean lon)
dx=distll_fun(lona,mean_lat,lonb,mean_lat);
dy=distll_fun(mean_lon,lata,mean_lon,latb);
totdist=distll_fun(lona,lata,lonb,latb);


%Turn this to a displacement in kilometers in x and y directions (with Y
%going from south to north and x going from west to east.
if(lata > latb)
    dy=-dy;
end
%With the longitudes we have to be more carefull.
if(abs(lona-lonb)> 180)
    if(lonb>lona)
        dx=-dx; %We went from 0 to 360 or from -180 to 180  
    end
else
    if(lona > lonb)
        dx=-dx;
    end
end

mag=totdist/deltat;
u=dx/deltat;
v=dy/deltat;

% if(totdist < 1000000)
% fprintf('%f  %f   %f   %f \n',lona,lata,lonb,latb)
% fprintf(' U = %f   , V= %f \n',u,v)
% fprintf(' DX= %f   ,DY= %f \n',dx,dy)
% end


end

