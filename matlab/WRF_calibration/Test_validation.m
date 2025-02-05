clear all
close all

year=2015;

Obs.Name='PER';
Obs.Data='D:\CAMMESA\Parque\salida_Parque_Fit.mat';
Obs.Graphics='D:\CAMMESA\graficos\';
Obs.Outputs='D:\CAMMESA\salidas\';

load (Obs.Data)

[Obs]=Get_Observation_function(Obs,Parque,year);

load 'datos_WRF.mat'

[Val]=Match_Data_function(Obs,WRF);

save('datos_val.mat','Val')
%% ========================================================================
% load 'datos_val.mat'
% =========================================================================


%% ========================================================================
[Val.RmseVel,Val.BiasVel,Val.RmseDir,Val.BiasDir,Val.RcoefVel,Val.RcoefDir,Val.PoliVel,Val.PoliDir]=Statistical_function(Val.WRFVel,Val.ObsVel,Val.WRFDir,Val.ObsDir);

% Trimestral
for j=1:4
    if j==1; date_filter=(Val.datetotal(:,2)==12 | Val.datetotal(:,2)==1 | Val.datetotal(:,2)==2);
    elseif j==2; date_filter=(Val.datetotal(:,2)==3 | Val.datetotal(:,2)==4 | Val.datetotal(:,2)==5);
    elseif j==3; date_filter=(Val.datetotal(:,2)==6 | Val.datetotal(:,2)==7 | Val.datetotal(:,2)==8);
    else date_filter=(Val.datetotal(:,2)==9 | Val.datetotal(:,2)==10 | Val.datetotal(:,2)==11);
    end
   
    [Val.RmseVelTri(:,j),Val.BiasVelTri(:,j),Val.RmseDirTri(:,j),Val.BiasDirTri(:,j),Val.RcoefVel(:,:,:,j),Val.RcoefDir(:,:,:,j),Val.PoliVel(:,:,:,j),Val.PoliDir(:,:,:,j)]=Statistical_function(Val.WRFVel(date_filter,:),Val.ObsVel(date_filter,:),Val.WRFDir(date_filter,:),Val.ObsDir(date_filter,:));

end

figure
for idir=1:4;
        if idir==1; var=Val.RmseVel; tit='RMSE Vel';
        elseif idir==2; var=Val.RmseDir; tit='RMSE Dir';
        elseif idir==3; var=Val.BiasVel; tit='BIAS Vel';
        else var=Val.BiasDir; tit='BIAS Dir'; 
        end   
        subplot(2,2,idir)
        plot(var)
        hold on
        title(tit)
        axis([0 length(var) min(var) max(var)]);  
end
        

Plot_Tri_function(Val.RmseVelTri,'RMSE Vel'); 
Plot_Tri_function(Val.BiasVelTri,'BIAS Vel');       
Plot_Tri_function(Val.RmseDirTri,'RMSE Dir'); 
Plot_Tri_function(Val.BiasDirTri,'BIAS Dir'); 

