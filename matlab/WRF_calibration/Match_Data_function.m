function [struct]=Match_Data_function(data,model)

j=1;k=1;
for i=1:length(data.Date)
    if (data.Date(i,:)==model.Date(j,:))
        struct.date(k,:)=data.Date(i,:);
    else 
        struct.date(k,:)=NaN;
        j=j-1;
    end
    k=k+1;
    j=j+1;
end

[ii,jj]=find(isnan(struct.date));
struct.datetotal=struct.date;
struct.datetotal(ii,:)=[];
struct.ObsVel=data.VEL;
struct.ObsDir=data.DIR;
struct.ObsVel(ii,:)=[];
struct.ObsDir(ii,:)=[];
struct.WRFVel=model.Data.TS(:,:,15);
struct.WRFDir=model.Data.TS(:,:,16);

return