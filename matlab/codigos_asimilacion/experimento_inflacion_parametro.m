clear all
close all

%En este experimento vamos a calcular el RMSE del estado y del parametro al
%igual que su dispersion analizar si funciona el metodo de estimacion de la
%inflacion y como es su desempenio para distintos tamanios del ensamble y
%spread iniciales.

ensemble_members=[3 6 10 15 20 30 40 50 75 100 200];
sigma_factor=[0.01 0.1 0.5 0.75 1.0 1.25 1.5 2];

ekf_sigmapar=[0.24 0.03 0.1];  %Spread de referencia que obtuvimos con el EKF (usando el modelo adjunto).


for ii=1:length(sigma_factor)
    
    for jj=1:length(ensemble_members)
        sigmapar=ekf_sigmapar*sigma_factor(ii);
        display(['Haciendo experimento correspondiente a ii=' num2str(ii) ' y jj= ' num2str(jj)])
        tic
        [rmse_a_fix(ii,jj,:),rmse_f_fix(ii,jj,:),sprd_a_fix(ii,jj,:,:),sprd_f_fix(ii,jj,:,:),mean_inflation_fix(ii,jj)]=function_etkf_parameters_parfixedsprd(ensemble_members(jj),sigmapar);
        [rmse_a_est(ii,jj,:),rmse_f_est(ii,jj,:),sprd_a_est(ii,jj,:,:),sprd_f_est(ii,jj,:,:),mean_inflation_est(ii,jj)]=function_etkf_parameters_parestinf(ensemble_members(jj),sigmapar);
        toc
    end
end


save('experimento_inflacion.mat');