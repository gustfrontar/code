load worldlo.mat

lat_costa=POline(2).lat;
lon_costa=POline(2).long;

lat_pais=POline(1).lat;
lon_pais=POline(1).long;

lat_rios=DNline(1).lat;
lon_rios=DNline(1).long;

lat_lagos=DNline(2).lat;
lon_lagos=DNline(2).long;

lat_ciudad=PPpoint(1).lat;
lon_ciudad=PPpoint(1).long;


%plot(lon_costa,lat_costa,'k')
%hold on
%plot(lon_pais,lat_pais,'k')
%plot(lon_rios,lat_rios,'b')
%plot(lon_lagos,lat_lagos,'b')
%plot(lon_ciudad,lat_ciudad,'ro')
