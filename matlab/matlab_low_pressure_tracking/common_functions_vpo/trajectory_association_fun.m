

function [is_associated]=trajectory_association_fun(trajectory1,trajectory2)
%This function checks is two given trayectories are associated using a time
%overlapping and a possition criteria. 
%This function is designed to be used with an analyzed trajectory and a
%forecasted trajectory.
is_associated=false;  %Initialize output.
T=60;                 %Time superpossition threshold.
D=500*1e3;            %Distance threshold in Km.
K=4;                  %Number of points where the distance criteria will be applied.

%==========================================================================
% Apply two criteria, one based on time overlap and the second based on
% distance.
%==========================================================================


if( trajectory1.enddate < trajectory2.startdate || trajectory2.enddate < trajectory1.startdate)
    %No time overlap.
    return
end


%Compute the overlaping period.

[common_dates overlap1 overlap2]=intersect(trajectory1.daten,trajectory2.daten);

overlap_size=length(common_dates);

if( 100*(2*ovelap_size/(trajectory1.length+trajectory2.length)) < T)
    %Time overlap is less than required.
    return
end

%Apply the distance criteria (distance of the kth first points within the
%overlaping period).

K=min([K overlap_size]);

lat1=trajectory1.minlatf(overlap1);
lon1=trajectory1.minlonf(overlap1);
lat2=trajectory2.minlatf(overlap2);
lon2=trajectory2.minlonf(overlap2);

for ii=1:K
   dist=disll_fun(lon1(ii),lat1(ii),lon2(ii),lat2(ii));
   if(dist > D)
       %The distance criteria has not been fullfiled.
       return
   end
    
end

%If the execution reached this point is because the systemas are
%associated.
is_associated=true;

end






