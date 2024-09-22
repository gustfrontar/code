function   [rank_lon]=rank_lon_fun(ens,obs)
%Esta funcion calcula el rank entre un grupo de miembros del ensamble y la
%observacion para las longitudes, teniendo en cuenta que puede estar la
%linea de cambio de fecha o grenwich en el medio.

if(size(ens,1)==1)
    ens=ens';
end

rank_lon=NaN(size(obs));

for ii=1:length(obs)

if(max(ens(:,ii))-min(ens(:,ii)) > 180 || obs(ii) - max(ens(:,ii)) > 180 || obs(ii) - min(ens(:,ii)) > 180 )
    if(max(ens(:,ii)) > 180 || obs(ii) > 180 )
      tmp=ens(:,ii);
      tmp(tmp > 180)=tmp(tmp > 180)-360;
      if(obs(ii) > 180)
          obs(ii)=obs(ii)-360;
      end
      rank_lon(ii)=sum(obs(ii) > tmp)+1;
      

     elseif(min(ens(:,ii)) < 0 || obs(ii) < 0)
         tmp=ens(:,ii);
         tmp(tmp < 0)=tmp(tmp < 0)+360;
         if(obs(ii) < 0)
             obs(ii)=obs(ii)+360;
         end
         rank_lon(ii)=sum(obs(ii) > tmp)+1;
    end
else
           
    rank_lon(ii)=sum(obs(ii) > ens(:,ii))+1;
end

end