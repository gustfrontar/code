
function [ verification ]=VerificationFun(for_data_vel,obs_data_vel,for_data_dir,obs_data_dir,for_data_pow,obs_data_pow)
%Inputs:
%for_data_vel un array conteniendo los pronosticos del WRF. Cada fila es un pronostico, cada columna representa un plazo de pronostico. 
 
%obs_data_vel un array conteniendo los valores de viento observados en la torre del parque que corresponde a los valores pronosticados por el WRF. Es decir que corresponde a la misma fecha que los pronosticos realizados por el WRF.
% dates es un array conteniendo la informacion de las fechas.

% nanrows=any( isnan(for_data_vel) , 2) | any(isnan(obs_data_vel),2)  ; % | any(isnan(obs_data_pow) , 2)  ;
% 
% for_data_vel(nanrows,:)=[];
% obs_data_vel(nanrows,:)=[];
% 
% for_data_dir(nanrows,:)=[];
% obs_data_dir(nanrows,:)=[];
% 
% for_data_pow(nanrows,:)=[];
% obs_data_pow(nanrows,:)=[];
% 
nlead=size(for_data_vel,2);
nfor=size(for_data_vel,1);


%==========================================================================
%CALCULO LOS ERRORES PARA LA POTENCIA
%==========================================================================

verification.PowRmse= sqrt( nanmean( ( obs_data_pow - for_data_pow ).^2 , 1 ) ) ;

verification.PowBias= nanmean( ( obs_data_pow - for_data_pow ) , 1 )  ;

verification.PowRmsedebias= sqrt( nanmean( ( obs_data_pow - for_data_pow ).^2 , 1 ) - verification.PowBias );

verification.PowMeanObs=nanmean( obs_data_pow , 1 );
verification.PowMeanFor=nanmean( for_data_pow , 1 );

verification.PowStdObs=nanstd( obs_data_pow , [] , 1 );
verification.PowStdFor=nanstd( for_data_pow , [] , 1 );


%Calculo el coeficiente de correlacion y el ajuste lineal.

for il=1:nlead

  tmpfor=for_data_pow(:,il);
  tmpobs=obs_data_pow(:,il);
  
  NaNIndex= isnan(tmpfor) | isnan(tmpobs) ;
  tmpfor(NaNIndex)=[];
  tmpobs(NaNIndex)=[];
  
  tmp=corrcoef(tmpfor,tmpobs);
  
  verification.PowCorrelation(il)=tmp(1,2);

  verification.PowP(:,il)=polyfit(tmpfor,tmpobs,1);

end


MaxPow=max(max(obs_data_pow));
PowRes=MaxPow/20;
leadres=3;

powindex=1;
for ipow=0:PowRes:MaxPow
        
 max_pow=ipow + PowRes*2 ;
 min_pow=ipow - PowRes*2 ;

 for il=1:nlead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > nlead);maxlead=nlead;end
        
        TmpPowObs=reshape(obs_data_pow(:,minlead:maxlead),[1 numel(obs_data_pow(:,minlead:maxlead))]);
        TmpPowFor=reshape(for_data_pow(:,minlead:maxlead),[1 numel(for_data_pow(:,minlead:maxlead))]);
        
        PowIndex= TmpPowFor > min_pow & TmpPowFor < max_pow ;
        
        verification.PowConditionalRmse(powindex,il)=sqrt( nanmean( (TmpPowFor(PowIndex) - TmpPowObs(PowIndex) ).^2 ) ) ;
        verification.PowConditionalBias(powindex,il)= nanmean( TmpPowFor(PowIndex) - TmpPowObs(PowIndex) );
        verification.PowConditionalStdError(powindex,il)= nanstd( TmpPowFor(PowIndex) - TmpPowObs(PowIndex) );
        verification.PowConditionalN(powindex,il)=sum( PowIndex);
        verification.PowConditionalPow(powindex)=ipow;
    
 end
 
 powindex=powindex+1;
 
end



%==========================================================================
%CALCULO LOS ERRORES PARA LA INTENSIDAD
%==========================================================================

verification.VelRmse= sqrt( nanmean( ( obs_data_vel - for_data_vel ).^2 , 1 ) ) ;

verification.VelBias= nanmean( ( obs_data_vel - for_data_vel ) , 1 )  ;

verification.VelRmsedebias= sqrt( nanmean( ( obs_data_vel - for_data_vel ).^2 , 1 ) - verification.VelBias );

verification.VelMeanObs=nanmean( obs_data_vel , 1 );
verification.VelMeanFor=nanmean( for_data_vel , 1 );

verification.VelStdObs=nanstd( obs_data_vel , [] , 1 );
verification.VelStdFor=nanstd( for_data_vel , [] , 1 );


%Calculo el coeficiente de correlacion y el ajuste lineal.

for il=1:nlead

  tmpfor=for_data_vel(:,il);
  tmpobs=obs_data_vel(:,il);
  
  NaNIndex= isnan(tmpfor) | isnan(tmpobs) ;
  tmpfor(NaNIndex)=[];
  tmpobs(NaNIndex)=[];

  tmp=corrcoef(tmpfor,tmpobs);
  
  verification.VelCorrelation(il)=tmp(1,2);

  verification.VelP(:,il)=polyfit(tmpfor,tmpobs,1);

end


MaxVel=25;
VelRes=MaxVel/20;
leadres=3;

velindex=1;
for ivel=0:VelRes:MaxVel
        
 max_vel=ivel + VelRes*2 ;
 min_vel=ivel - VelRes*2 ;

 for il=1:nlead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > nlead);maxlead=nlead;end
        
        TmpVelObs=reshape(obs_data_vel(:,minlead:maxlead),[1 numel(obs_data_vel(:,minlead:maxlead))]);
        TmpVelFor=reshape(for_data_vel(:,minlead:maxlead),[1 numel(for_data_vel(:,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        verification.VelConditionalRmse(velindex,il)=sqrt( nanmean( ( TmpVelFor(VelIndex) - TmpVelObs(VelIndex) ).^2 ) ) ;
        verification.VelConditionalBias(velindex,il)= nanmean( TmpVelFor(VelIndex) - TmpVelObs(VelIndex) );
        verification.VelConditionalStdError(velindex,il)= nanstd( TmpVelFor(VelIndex) - TmpVelObs(VelIndex) );
        verification.VelConditionalN(velindex,il)=sum( VelIndex);
        verification.VelConditionalVel(velindex)=ivel;
    
 end
 
 velindex=velindex+1;
 
end


%==========================================================================
%CALCULO LOS ERRORES PARA LA DIRECCION
%==========================================================================

%Primero corrijo los pronosticos para evitar que la discontinuidad en 0 afecte los scores.

DirDiff=for_data_dir - obs_data_dir;

for ifor=1:nfor
 for il=1:nlead
 
    if( DirDiff(ifor,il) > 180 )
      DirDiff(ifor,il) =  DirDiff(ifor,il) - 360 ;
    end
 
    if( DirDiff(ifor,il) < -180 )
      DirDiff(ifor,il) = DirDiff(ifor,il) + 360 ;
    end


 end
end


%Una vez hecho esto podemos calcular de manera segura los scores para la direccion.


%Calculo el RMSE y el bias

verification.DirRmse= sqrt( nanmean( ( DirDiff ).^2 , 1 ) ) ;

verification.DirBias= nanmean( ( DirDiff ) , 1 )  ;

verification.DirRmsedebias= sqrt( nanmean( ( DirDiff ).^2 , 1 ) - verification.DirBias );

verification.DirMeanObs=nanmean( obs_data_dir , 1 );
verification.DirMeanFor=nanmean( for_data_dir , 1 );

verification.DirStdObs=nanstd( obs_data_dir , [] , 1 );
verification.DirStdFor=nanstd( for_data_dir , [] , 1 );

%Calculo el coeficiente de correlacion y el ajuste lineal.

for il=1:nlead

  tmpfor=obs_data_dir(:,il)+DirDiff(:,il);
  tmpobs=obs_data_dir(:,il);
  
  
  
  NaNIndex= isnan(tmpfor) | isnan(tmpobs) ;
  tmpfor(NaNIndex)=[];
  tmpobs(NaNIndex)=[];

  verification.DirCorrelation(il)=CorrelationDir(tmpfor,tmpobs);

  verification.DirP(:,il)=polyfit(tmpfor,tmpobs,1);

end




