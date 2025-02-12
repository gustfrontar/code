function [forecast_struct observation_struct]=simple_match_fun(forecast_struct,observation_struct)
dx=4; %Delta x en km.
distance_factor_threshold=1.4;
%This function applies a simple matching criteria between precipitating
%systems.


%Lets loop over the forecasted systems and over the observed systems to
%perform the match. Only distance between centroids is going to be taken
%into account!! (Further properties can also be included in the matching
%function as in MODE, the difficult part is to set the relative
%contribution of each propertie since they are not measured in the same
%units).

%Each system (forecasted and observed) have their mutual distance factors.
%We will match the systems based on this information of mutual distance.

for ifor=1:forecast_struct.nsistemas  
           forecast_struct.systems(ifor).matched=false;
           forecast_struct.systems(ifor).distance_factor=NaN;
           forecast_struct.systems(ifor).matchedsys=NaN;
end
for iobs=1:observation_struct.nsistemas
           observation_struct.systems(iobs).matched=false;
           observation_struct.systems(iobs).distance_factor=NaN;
           observation_struct.systems(iobs).matchedsys=NaN;
end

%Distance factor is computed based on cluster distance and also on cluster
%size. Distance factor in this script can be replaced by more complex
%functions that incorporate other system attributes.

%Compute mutual distance factors.
for ifor=1:forecast_struct.nsistemas  
   for iobs=1:observation_struct.nsistemas
       d1=sqrt(forecast_struct.systems(ifor).gridsize*dx*dx/pi)*1000; %Equivalent radius in meters for forecasted system.
       d2=sqrt(observation_struct.systems(iobs).gridsize*dx*dx/pi)*1000;
       lon1=forecast_struct.systems(ifor).centroidlon;
       lat1=forecast_struct.systems(ifor).centroidlat;
       lon2=observation_struct.systems(iobs).centroidlon;
       lat2=observation_struct.systems(iobs).centroidlat;  
       [d]=distll_fun(lon1,lat1,lon2,lat2);
       distance_factor=d/(d1+d2);
       if ( distance_factor < distance_factor_threshold)
        if(~forecast_struct.systems(ifor).matched)
          if(~observation_struct.systems(iobs).matched)
          %THe observed system is free. Match the system
           forecast_struct.systems(ifor).matched=true;
           forecast_struct.systems(ifor).distance_factor=distance_factor;
           forecast_struct.systems(ifor).matchedsys=iobs;
           observation_struct.systems(iobs).matched=true;
           observation_struct.systems(iobs).distance_factor=distance_factor;
           observation_struct.systems(iobs).matchedsys=ifor;
          end
          if(observation_struct.systems(iobs).matched & observation_struct.systems(iobs).distance_factor > distance_factor)
          %THe observed system is not free but the proposed match is
          %better.
           %Dematch forecast associated with observation iobs
           aux=observation_struct.systems(iobs).matchedsys;
           forecast_struct.systems(aux).matched=false;
           forecast_struct.systems(aux).distance_factor=Nan;
           forecast_struct.systems(aux).matchedsys=NaN;
           %re match observation.
           observation_struct.systems(iobs).matched=true;
           observation_struct.systems(iobs).distance_factor=distance_factor;
           observation_struct.systems(iobs).matchedsys=ifor;
        
           forecast_struct.systems(ifor).matched=true;
           forecast_struct.systems(ifor).distance_factor=distance_factor;
           forecast_struct.systems(ifor).matchedsys=iobs;

          end
        end
        if(~forecast_struct.systems(ifor).matched & forecast_struct.systems(ifor).distance_factor > distance_factor)
          if(~observation_struct.systems(iobs).matched)
          %THe observed system is free. Match the system
           %De match previously matched system.
           aux=forecast_struct.systems(iobs).matchedsys;
           observation_struct.systems(aux).matched=false;
           observation_struct.systems(aux).distance_factor=NaN;
           observation_struct.systems(aux).matchedsys=NaN;   
          
           forecast_struct.systems(ifor).matched=true;
           forecast_struct.systems(ifor).distance_factor=distance_factor;
           forecast_struct.systems(ifor).matchedsys=iobs;
           observation_struct.systems(iobs).matched=true;
           observation_struct.systems(iobs).distance_factor=distance_factor;
           observation_struct.systems(iobs).matchedsys=ifor;
          end
          if(observation_struct.systems(iobs).matched & observation_struct.systems(iobs).distance_factor > distance_factor)
          %THe observed system is not free but the proposed match is
          %better.
           %Dematch observation associated with ifor
           aux=forecast_struct.systems(iobs).matchedsys;
           observation_struct.systems(aux).matched=false;
           observation_struct.systems(aux).distance_factor=NaN;
           observation_struct.systems(aux).matchedsys=NaN; 
           %Dematch forecast associated with observation iobs
           aux=observation_struct.systems(iobs).matchedsys;
           forecast_struct.systems(aux).matched=false;
           forecast_struct.systems(aux).distance_factor=Nan;
           forecast_struct.systems(aux).matchedsys=NaN;
           
           %re match observation and forecast.
           observation_struct.systems(iobs).matched=true;
           observation_struct.systems(iobs).distance_factor=distance_factor;
           observation_struct.systems(iobs).matchedsys=ifor;
        
           forecast_struct.systems(ifor).matched=true;
           forecast_struct.systems(ifor).distance_factor=distance_factor;
           forecast_struct.systems(ifor).matchedsys=iobs;

          end        
  
        end
       end
   end
end


