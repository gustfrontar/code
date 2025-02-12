clear all
close all


load ../EXPERIMENTOS/CONTROL40M_IDEAL_CONSTANTPAR_MEMNC/verification_fnl/rmse_bias.mat

TOTALRMSECTRL=squeeze(squeeze(nanmean(RMSEU,3)+nanmean(RMSEV,3)+nanmean(RMSET,3))+(RMSEP));

load ../EXPERIMENTOS/QFX0DNOZLOC40M_IDEAL_CONSTANTPAR_MEMNC/verification_fnl/rmse_bias.mat

TOTALPE=squeeze(squeeze(nanmean(RMSEU,3)+nanmean(RMSEV,3)+nanmean(RMSET,3))+(RMSEP));

pcolor((TOTALRMSEPE-TOTALRMSECTRL)/TOTALRMSEPE);

