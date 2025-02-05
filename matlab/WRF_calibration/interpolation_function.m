function [struct turbines]=interpolation_function(obs,model,turbines,option)

%% Datos de velocidad y direccion del viento del pronostico del WRF
forecast(1,:)=model.Data.TS(:,:,15);
forecast(2,:)=model.Data.TS(:,:,16);

%% Calculos de las interpolaciones segun la opcion elegida
for i=1:length(obs.Turbine)
    switch option
        case ('Mean')
            [struct.Vel(:,i)]=veldir_inter_function(forecast,obs.Turbine(i).DirVelMean,obs.Turbine(i).DirPowerMean,obs.Directions);
        case ('Mode')
            [struct.Vel(:,i)]=veldir_inter_function(forecast,obs.Turbine(i).DirVelMode,obs.Turbine(i).DirPowerMode,obs.Directions);
        case ('Median')
            [struct.Vel(:,i)]=veldir_inter_function(forecast,obs.Turbine(i).DirVelMedian,obs.Turbine(i).DirPowerMedian,obs.Directions);
        case ('Prct')
            [struct.Vel(:,i)]=veldir_inter_function(forecast,obs.Turbine(i).DirVelPrct,obs.Turbine(i).DirPowerPrct,obs.Directions);
        case ('Parametric')
            [struct.Vel(:,i)]=veldir_inter_function(forecast,obs.Turbine(i).DirParametric,obs.Turbine(i).DirFun,obs.Directions);
    end  
    % Mascara de las turbinas faltantes
    if(find(i==turbines)~=0)
        disp('entro')
        mask=zeros(length(struct.Vel),1);
    else
        disp('no entro')
        mask=ones(length(struct.Vel),1);   
    end
    struct.Vel(:,i)=struct.Vel(:,i).*mask;
    % Vector con numero de turbinas
    struct.turbines(i)=i;  
end
return