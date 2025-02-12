clear all
close all

PARAMETER='RHBL'
min_par=0.5
max_par=1.0

NPAR=32;   %Numero de valores diferentes que toman los parametros para cada fecha.
EXPPATH=['/home/jruiz/DATA/EXP_SPEEDY/'];

for ii=1:NPAR
   par(ii)=(ii-1)*(max_par-min_par)/(NPAR-1)+min_par;
end

load('/home/jruiz/DATA/EXP_SPEEDY/PERFECT_CO2X2SSTSKT__1_196901_360_1/climatology.mat');
    clim_mean_perfect=clim_mean;
    var_mean_perfect=var_mean;
    trend_mean_perfect=trend_mean;

load('/home/jruiz/DATA/EXP_SPEEDY/PERFECT_PI__1_196901_360_1/climatology.mat');
    clim_mean_perfect=clim_mean_perfect-clim_mean;
    var_mean_perfect=var_mean_perfect-var_mean;
    trend_mean_perfect=trend_mean_perfect-trend_mean;

%Read J

clim_mean_exp=NaN([size(clim_mean_perfect) NPAR]);
var_mean_exp=NaN([size(var_mean_perfect) NPAR]);
trend_mean_exp=NaN([size(trend_mean_perfect) NPAR]);

%COMPUTE COST FUNCTION AND MODEL SENSITIVITY

  for ipar=1:NPAR
ipar
       %Calculo el error medio.
       if(ipar < 10)
         spar=[ '00' num2str(ipar) ];
       end
       if(ipar < 100 & ipar >= 10)
         spar=[ '0' num2str(ipar) ];
       end
       load([EXPPATH '/CLIMATOLOGY30_' PARAMETER '_CO2X2SST_' spar  '_1_196901_360_1/climatology.mat']);
       clim_mean_exp(:,:,:,:,ipar)=clim_mean;
       var_mean_exp(:,:,:,:,ipar)=var_mean;
       trend_mean_exp(:,:,:,:,ipar)=trend_mean;

       load([EXPPATH '/CLIMATOLOGY30_' PARAMETER '_' spar  '_1_196901_360_1/climatology.mat']);

       clim_mean_exp(:,:,:,:,ipar)=clim_mean_exp(:,:,:,:,ipar)-clim_mean;
       var_mean_exp(:,:,:,:,ipar)=var_mean_exp(:,:,:,:,ipar)-var_mean;
       trend_mean_exp(:,:,:,:,ipar)=trend_mean_exp(:,:,:,:,ipar)-trend_mean;

       %Cost function (absolute difference in the climatological mean, variance and physics trends)    
       J_clim(:,:,ipar)=(squeeze(nanmean(nanmean(abs(clim_mean_exp(:,:,:,:,ipar)-clim_mean_perfect),2),1)));
       J_var(:,:,ipar)=(squeeze(nanmean(nanmean(abs(var_mean_exp(:,:,:,:,ipar)-var_mean_perfect),2),1)));
       J_trend(:,:,ipar)=(squeeze(nanmean(nanmean(abs(trend_mean_exp(:,:,:,:,ipar)-trend_mean_perfect),2),1)));

       %Cost function in the tropics (absolute difference in the climatological mean, variance and physics trends)    
       local=20:29;
       Jt_clim(:,:,ipar)=(squeeze(nanmean(nanmean(abs(clim_mean_exp(local,:,:,:,ipar)-clim_mean_perfect(local,:,:,:)),2),1)));
       Jt_var(:,:,ipar)=(squeeze(nanmean(nanmean(abs(var_mean_exp(local,:,:,:,ipar)-var_mean_perfect(local,:,:,:)),2),1)));
       Jt_trend(:,:,ipar)=(squeeze(nanmean(nanmean(abs(trend_mean_exp(local,:,:,:,ipar)-trend_mean_perfect(local,:,:,:)),2),1)));

       %Sensitivity (averaged difference in the climatological mean, variance and physics trends)
       S_clim(:,:,ipar)=(squeeze(nanmean(nanmean(clim_mean_exp(:,:,:,:,ipar)-clim_mean_perfect,2),1)));
       S_var(:,:,ipar)=(squeeze(nanmean(nanmean(var_mean_exp(:,:,:,:,ipar)-var_mean_perfect,2),1)));
       S_trend(:,:,ipar)=(squeeze(nanmean(nanmean(trend_mean_exp(:,:,:,:,ipar)-trend_mean_perfect,2),1)));

       %Sensitivity in the tropics (averaged difference in the climatological mean, variance and physics trends)
       local=20:29;
       St_clim(:,:,ipar)=(squeeze(nanmean(nanmean(clim_mean_exp(local,:,:,:,ipar)-clim_mean_perfect(local,:,:,:),2),1)));
       St_var(:,:,ipar)=(squeeze(nanmean(nanmean(var_mean_exp(local,:,:,:,ipar)-var_mean_perfect(local,:,:,:),2),1)));
       St_trend(:,:,ipar)=(squeeze(nanmean(nanmean(trend_mean_exp(local,:,:,:,ipar)-trend_mean_perfect(local,:,:,:),2),1)));

   end

%COMPUTE VARIABILITY ASSOCIATED TO THE PARAMETER AND LINEAR RELATION STRENGTH.
display('Computing variability associated with the parameter')
std_clim=std(clim_mean_exp,[],5);
std_var=std(var_mean_exp,[],5);
std_trend=std(trend_mean_exp,[],5);

std_par=std(par,[]);
mean_par=mean(par);
mean_clim=mean(clim_mean_exp,5);
mean_var =mean(var_mean_exp,5);
mean_trend=mean(trend_mean_exp,5);

display('Computing correlation associated with the parameter')

tmp1=zeros(size(mean_clim));
tmp2=zeros(size(mean_var));
tmp3=zeros(size(mean_trend));
for m=1:size(clim_mean_exp,5)
    m
    tmp1=tmp1+(clim_mean_exp(:,:,:,:,m)-mean_clim)*(par(m)-mean_par);
    tmp2=tmp2+(var_mean_exp(:,:,:,:,m)-mean_var)*(par(m)-mean_par);
    tmp3=tmp3+(trend_mean_exp(:,:,:,:,m)-mean_trend)*(par(m)-mean_par);
end

corrcoef_clim=tmp1./((std_par)*(std_clim)*NPAR);
corrcoef_var=tmp2./((std_par)*(std_var)*NPAR);
corrcoef_trend=tmp3./((std_par)*(std_trend)*NPAR);

save(['clim_cost_function_PERFECT_CLIMATECHANGE_' PARAMETER '.mat'],'par','J_clim','J_var','J_trend','Jt_clim','Jt_var','Jt_trend','S_clim','S_var','S_trend','St_clim','St_var','St_trend','corrcoef_clim','corrcoef_var','corrcoef_trend','std_clim','std_var','std_trend')

