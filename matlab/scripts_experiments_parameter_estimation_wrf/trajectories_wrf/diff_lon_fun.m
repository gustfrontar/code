function   [difflon]=diff_lon_fun(lon1,lon2)

%NaN Ready 
for ii=1:length(lon1)
   lon(ii,1)=lon1(ii);
   lon(ii,2)=lon2(ii);
end

difflon=NaN(size(lon1));
%lon=[lon1 lon2];
%Esta funcion calcula la diferencia entre 2 longitudes teniendo en cuenta
%la linea de cambio de fecha o eventualmente el meridiano de Greenwich
for ii=1:length(lon1)

if(max(lon(ii,:))-min(lon(ii,:)) > 180)
    if(max(lon(ii,:)) > 180)
      tmp=lon(ii,:);
      tmp(tmp > 180)=tmp(tmp > 180)-360;
      difflon(ii)=tmp(1)-tmp(2);

     elseif(min(lon(ii,:)) < 0)
         tmp=lon(ii,:);
         tmp(tmp < 0)=tmp(tmp < 0)+360;
         difflon(ii)=tmp(1)-tmp(2);
    end
       else
    difflon(ii)=lon(ii,1)-lon(ii,2);
end

end