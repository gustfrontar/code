function   [meanlon]=mean_lon_fun(lon)

%NaN Ready 

%This function computes the average longitud over an ensemble of point
%taking into account that the date change line could be somewhere within
%the ensemble.
%lon is a 1D array with the list of longitudes to be averaged.
%Me quedo con los puntos que no son NaN;
lon=lon(~isnan(lon));

if(max(lon)-min(lon) > 180)
    if(max(lon) > 180)
      lon(lon > 180)=lon(lon > 180)-360;
      meanlon=mean(lon);
       if(meanlon < 0)
         meanlon=meanlon+360;
       end
       elseif(min(lon) < 0)
         lon(lon < 0)=lon(lon < 0)+360;
         meanlon=mean(lon);
       if(meanlon > 180)
         meanlon=meanlon-360;
       end 
    end
    
    
       else
    meanlon=mean(lon);
end