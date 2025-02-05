function [struct]=Get_Observation_function(struct,data,year)

% Acumula las observaciones segun sea una hora o 10 minutos

if data.Accumulation==1
    j=1;
    for i=2:24:length(data.Vel)
        struct.Vel(j,:)=data.Vel(i:i+23)';
        struct.Dir(j,:)=data.Dir(i:i+23)';
        struct.Date(j,:)=data.Date(i-1,:);
        j=j+1;
    end
    Aux=[[struct.Vel(2:end,:);NaN(1,size(struct.Vel,2))],[struct.Vel(3:end,1:3);NaN(2,3)]];
    struct.VEL=[struct.Vel Aux];
    Aux=[[struct.Dir(2:end,:);NaN(1,size(struct.Dir,2))],[struct.Dir(3:end,1:3);NaN(2,3)]];
    struct.DIR=[struct.Dir Aux];
     
else 
    j=1;
    for i=2:144:length(data.Vel)
        struct.Vel(j,:)=data.Vel(i:i+143)';
        struct.Dir(j,:)=data.Dir(i:i+143)';
        struct.Date(j,:)=data.Date(i-1,:);
        j=j+1;
    end
    Aux=[[struct.Vel(2:end,:);NaN(1,size(struct.Vel,2))],[struct.Vel(3:end,1:18);NaN(2,18)]];
    struct.VEL=[struct.Vel Aux];
    Aux=[[struct.Dir(2:end,:);NaN(1,size(struct.Dir,2))],[struct.Dir(3:end,1:18);NaN(2,18)]];
    struct.DIR=[struct.Dir Aux];   
end

date_filter=(struct.Date(:,1)==year);
struct.VEL=struct.VEL(date_filter,:);
struct.DIR=struct.DIR(date_filter,:);
struct.Date=struct.Date(date_filter,:);

return
 
