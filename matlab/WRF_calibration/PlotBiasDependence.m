clear all
close all

%==========================================================================
% Este script realiza una verificacion "Offline" de los pronosticos de
% viento y potencia y ademas compara los resultados con los obtenidos a
% partir de un pronostico calibrado estadisticamente.
%==========================================================================

%Cargamos un conjunto de datos de pronosticos y observaciones. Este archivo
%contiene una estructura Val que tiene 4 arrays (pronosticos del WRF para
%velocidad y direccion y observaciones del viento medio del parque para
%velocidad y direccion)

doyres=30;        %Resolucion en epoca del anio
leadres=2;        %Resolucion en plazo de pronostico
dirres=30;        %Resolucion en direccion.
velres=2.5;       %Resolucion en velocidad.

MinSampleSize=200;

load CalibrationOutput.mat;

VelFor=VelUncalibrated;
DirFor=DirUncalibrated;

VelForCalibrated=VelCalibratedDoyLeadVel;
DirForCalibrated=DirCalibratedDoyLeadVel;

Date=Val.datetotal;

%Calculo el dia del anio para toda la muestra
DateNum=datenum( Date );
doy = DateNum - datenum(year(DateNum),1,1) + 1;  %Day of year de cada pronostico en la base de datos

%Obtengo la cantidad de plazos de pronostico.
NLead=size(VelFor,2);  %Cantidad de plazos de pronostico.

%==========================================================================
%ANALIZO LA CLIMATOLOGIA, COMO DEPENDE LA MEDIA DEL VIENTO Y LA DIRECCION
%CON LA EPOCA DEL ANIO Y LA HORA DEL DIA. TAMBIEN ANALIZO SU VARIABILIDAD.
%==========================================================================
  
for idoy=1:365
        
 max_doy=idoy + doyres ;
 min_doy=idoy - doyres ;

 if( max_doy > 365)
   %El tope del perido se paso para el anio siguiente.
   max_doy = max_doy - 365;
   TimeWindowIndex = doy <= max_doy | doy >= min_doy;
 elseif( min_doy < 1)
   min_doy=min_doy + 365;
   TimeWindowIndex = doy >= min_doy | doy <= max_doy;
 else
   TimeWindowIndex = doy >= min_doy & doy <= max_doy;
 end


 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelObs=VelObs(TimeWindowIndex,minlead:maxlead);
        TmpDirObs=DirObs(TimeWindowIndex,minlead:maxlead);
        
        TmpVelObs=reshape(TmpVelObs,[1 numel(TmpVelObs)]);
        TmpDirObs=reshape(TmpDirObs,[1 numel(TmpDirObs)]);
        
     W=ones(size(TmpDirObs)); 
     [MeanDirDoyLeadObs(idoy,il) StdDirDoyLeadObs(idoy,il)]=MeanStdDir(TmpDirObs,W);
     
     MeanVelDoyLeadObs(idoy,il)=nanmean(TmpVelObs);
     StdVelDoyLeadObs(idoy,il)=nanstd(TmpVelObs);
       
 end
 
end


%Climatologia de las direcciones

MaxDir=360;

for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelObs=reshape(VelObs(:,minlead:maxlead),[1 numel(VelObs(:,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(:,minlead:maxlead),[1 numel(DirObs(:,minlead:maxlead))]);
        
        DirIndex= TmpDirObs >= min_dir & TmpDirObs <= max_dir ;
        
        MeanVelLeadDirObs(idir,il)=nanmean(TmpVelObs(DirIndex));
        StdVelLeadDirObs(idir,il)=nanstd(TmpVelObs(DirIndex));
        NLeadDirObs(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for idir=1:MaxDir

 max_dir=idir + dirres ;
 min_dir=idir - dirres ;
       
 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        DirIndex= TmpDirObs >= min_dir & TmpDirObs <= max_dir ;
        
        MeanVelLeadDirSummerObs(idir,il)=nanmean(TmpVelObs(DirIndex));
        StdVelLeadDirSummerObs(idir,il)=nanstd(TmpVelObs(DirIndex));
        NLeadDirSummerObs(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);

        DirIndex= TmpDirObs >= min_dir & TmpDirObs <= max_dir ;
        
        MeanVelLeadDirWinterObs(idir,il)=nanmean(TmpVelObs(DirIndex));
        StdVelLeadDirWinterObs(idir,il)=nanstd(TmpVelObs(DirIndex));
        NLeadDirWinterObs(idir,il)=sum(DirIndex);
         
 end
 
end

%Suavizo un poco los patrones de error para visualizar mas claramente la
%dependencia con la epoca del anio y el plazo de pronsotico (hora del dia)

MeanVelDoyLeadObs=SmoothMatrix(MeanVelDoyLeadObs,7,7);
StdVelDoyLeadObs=SmoothMatrix(StdVelDoyLeadObs,7,7);
MeanDirDoyLeadObs=SmoothMatrix(MeanDirDoyLeadObs,7,7);
StdDirDoyLeadObs=SmoothMatrix(StdDirDoyLeadObs,7,7);

MeanVelLeadDirObs( NLeadDirObs < MinSampleSize )=NaN;
StdVelLeadDirObs( NLeadDirObs < MinSampleSize )=NaN;

MeanVelLeadDirSummerObs( NLeadDirObs < MinSampleSize/2 )=NaN;
StdVelLeadDirSummerObs( NLeadDirObs < MinSampleSize/2 )=NaN;
MeanVelLeadDirWinterObs( NLeadDirObs < MinSampleSize/2 )=NaN;
StdVelLeadDirWinterObs( NLeadDirObs < MinSampleSize/2 )=NaN;

tmp=sum(NLeadDirObs,1);
tmp=repmat(tmp,[360 1]);
NLeadDirObs=NLeadDirObs./tmp;

tmp=sum(NLeadDirWinterObs,1);
tmp=repmat(tmp,[360 1]);
NLeadDirWinterObs=NLeadDirWinterObs./tmp;

tmp=sum(NLeadDirSummerObs,1);
tmp=repmat(tmp,[360 1]);
NLeadDirSummerObs=NLeadDirSummerObs./tmp;

horas=(1:NLead)*10/60;
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanVelDoyLeadObs);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
caxis([0 10])
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdVelDoyLeadObs);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanDirDoyLeadObs);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Direccion medio')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdDirDoyLeadObs);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la direccion')
colorbar

print('-dpng','ClimatologyVelDirDoyLead.png')

dirs=1:360;

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])

subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyVelDirLeadDir.png')


hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])

subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirSummerObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirSummerObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirSummerObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyVelDirLeadDirSummer.png')


hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])

subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirWinterObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirWinterObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirWinterObs);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyVelDirLeadDirWinter.png')


%==========================================================================
%ANALIZO LA CLIMATOLOGIA, COMO DEPENDE LA MEDIA DEL VIENTO Y LA DIRECCION
%CON LA EPOCA DEL ANIO Y LA HORA DEL DIA. TAMBIEN ANALIZO SU VARIABILIDAD.
%==========================================================================
  
for idoy=1:365
        
 max_doy=idoy + doyres ;
 min_doy=idoy - doyres ;

 if( max_doy > 365)
   %El tope del perido se paso para el anio siguiente.
   max_doy = max_doy - 365;
   TimeWindowIndex = doy <= max_doy | doy >= min_doy;
 elseif( min_doy < 1)
   min_doy=min_doy + 365;
   TimeWindowIndex = doy >= min_doy | doy <= max_doy;
 else
   TimeWindowIndex = doy >= min_doy & doy <= max_doy;
 end


 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=VelFor(TimeWindowIndex,minlead:maxlead);
        TmpDirFor=DirFor(TimeWindowIndex,minlead:maxlead);
        
        TmpVelFor=reshape(TmpVelFor,[1 numel(TmpVelFor)]);
        TmpDirFor=reshape(TmpDirFor,[1 numel(TmpDirFor)]);
        
     W=ones(size(TmpDirFor)); 
     [MeanDirDoyLeadFor(idoy,il) StdDirDoyLeadFor(idoy,il)]=MeanStdDir(TmpDirFor,W);
     
     MeanVelDoyLeadFor(idoy,il)=nanmean(TmpVelFor);
     StdVelDoyLeadFor(idoy,il)=nanstd(TmpVelFor);
     
     
%      if( idoy == 20 & il == 20*6 )
%               
%        plot(TmpVelFor,TmpDirFor)
%          
%      end
       
 end
 
end

%Climatologia de las direcciones

MaxDir=360;

for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(:,minlead:maxlead),[1 numel(VelFor(:,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(:,minlead:maxlead),[1 numel(DirFor(:,minlead:maxlead))]);
        
        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
        
        MeanVelLeadDirFor(idir,il)=nanmean(TmpVelFor(DirIndex));
        StdVelLeadDirFor(idir,il)=nanstd(TmpVelFor(DirIndex));
        NLeadDirFor(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for idir=1:MaxDir

 max_dir=idir + dirres ;
 min_dir=idir - dirres ;
       
 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        
        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
        
        MeanVelLeadDirSummerFor(idir,il)=nanmean(TmpVelFor(DirIndex));
        StdVelLeadDirSummerFor(idir,il)=nanstd(TmpVelFor(DirIndex));
        NLeadDirSummerFor(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);

        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
        
        MeanVelLeadDirWinterFor(idir,il)=nanmean(TmpVelFor(DirIndex));
        StdVelLeadDirWinterFor(idir,il)=nanstd(TmpVelFor(DirIndex));
        NLeadDirWinterFor(idir,il)=sum(DirIndex);
         
 end
 
end

%Suavizo un poco los patrones de error para visualizar mas claramente la
%dependencia con la epoca del anio y el plazo de pronsotico (hora del dia)
MeanVelDoyLeadFor=SmoothMatrix(MeanVelDoyLeadFor,7,7);
StdVelDoyLeadFor=SmoothMatrix(StdVelDoyLeadFor,7,7);
MeanDirDoyLeadFor=SmoothMatrix(MeanDirDoyLeadFor,7,7);
StdDirDoyLeadFor=SmoothMatrix(StdDirDoyLeadFor,7,7);

MeanVelLeadDirFor( NLeadDirFor < MinSampleSize )=NaN;
StdVelLeadDirFor( NLeadDirFor < MinSampleSize )=NaN;

MeanVelLeadDirSummerFor( NLeadDirFor < MinSampleSize/2 )=NaN;
StdVelLeadDirSummerFor( NLeadDirFor < MinSampleSize/2 )=NaN;
MeanVelLeadDirWinterFor( NLeadDirFor < MinSampleSize/2 )=NaN;
StdVelLeadDirWinterFor( NLeadDirFor < MinSampleSize/2 )=NaN;

tmp=sum(NLeadDirFor,1);
tmp=repmat(tmp,[360 1]);
NLeadDirFor=NLeadDirFor./tmp;

tmp=sum(NLeadDirWinterFor,1);
tmp=repmat(tmp,[360 1]);
NLeadDirWinterFor=NLeadDirWinterFor./tmp;

tmp=sum(NLeadDirSummerFor,1);
tmp=repmat(tmp,[360 1]);
NLeadDirSummerFor=NLeadDirSummerFor./tmp;


horas=(1:NLead)*10/60;
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])



subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanVelDoyLeadFor);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
caxis([0 10])
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdVelDoyLeadFor);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanDirDoyLeadFor);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Direccion medio')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdDirDoyLeadFor);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la direccion')
colorbar

print('-dpng','ClimatologyForVelDirDoyLead.png')

dirs=1:360;

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])

subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyForVelDirLeadDir.png')

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])

subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirSummerFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirSummerFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirSummerFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyForVelDirLeadDirSummer.png')

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 60])
subplot(3,1,1)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,MeanVelLeadDirWinterFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Velocidad Media')
colorbar

subplot(3,1,2)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,StdVelLeadDirWinterFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Variabilidad de la velocidad')
colorbar

subplot(3,1,3)
set(gca,'FontSize',fontsize);

pcolor(horas,dirs,NLeadDirWinterFor);shading flat
ylabel('Direcciones (grados)');
xlabel('Plazo de pronostico (horas)');
title('Frecuencia')
caxis([0 0.01])
colorbar

print('-dpng','ClimatologyForVelDirLeadDirWinter.png')

%==========================================================================
%==========================================================================
%==========================================================================
%ANALIZO LOS ERRORES EN VELOCIDAD Y DIRECCION PARA EL PRONOSTICO NO
%CALIBRADO
%==========================================================================
%==========================================================================
%==========================================================================

%==========================================================================
%ANALIZO LA DEPENENCIA CON LA EPOCA DEL ANIO Y EL PLAZO DE PRONOSTICO.
%==========================================================================
  
for idoy=1:365
        
 max_doy=idoy + doyres ;
 min_doy=idoy - doyres ;

 if( max_doy > 365)
   %El tope del perido se paso para el anio siguiente.
   max_doy = max_doy - 365;
   TimeWindowIndex = doy <= max_doy | doy >= min_doy;
 elseif( min_doy < 1)
   min_doy=min_doy + 365;
   TimeWindowIndex = doy >= min_doy | doy <= max_doy;
 else
   TimeWindowIndex = doy >= min_doy & doy <= max_doy;
 end


 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=VelFor(TimeWindowIndex,minlead:maxlead);
        TmpVelObs=VelObs(TimeWindowIndex,minlead:maxlead);
        TmpDirFor=DirFor(TimeWindowIndex,minlead:maxlead);
        TmpDirObs=DirObs(TimeWindowIndex,minlead:maxlead);
        
        TmpVelFor=reshape(TmpVelFor,[1 numel(TmpVelFor)]);
        TmpVelObs=reshape(TmpVelObs,[1 numel(TmpVelObs)]);
        TmpDirFor=reshape(TmpDirFor,[1 numel(TmpDirFor)]);
        TmpDirObs=reshape(TmpDirObs,[1 numel(TmpDirObs)]);
        
        
  [MeanErrorVelDoyLead(idoy,il) StdErrorVelDoyLead(idoy,il) MeanErrorDirDoyLead(idoy,il) StdErrorDirDoyLead(idoy,il) ]=StatisticsVelDirError(TmpVelFor,TmpDirFor,TmpVelObs,TmpDirObs);
  
       
 end
 

end

%Suavizo un poco los patrones de error para visualizar mas claramente la
%dependencia con la epoca del anio y el plazo de pronsotico (hora del dia)
MeanErrorVelDoyLead=SmoothMatrix(MeanErrorVelDoyLead,7,7);
StdErrorVelDoyLead=SmoothMatrix(StdErrorVelDoyLead,7,7);
MeanErrorDirDoyLead=SmoothMatrix(MeanErrorDirDoyLead,7,7);
StdErrorDirDoyLead=SmoothMatrix(StdErrorDirDoyLead,7,7);


horas=(1:NLead)*10/60;
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanErrorVelDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdErrorVelDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanErrorDirDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdErrorDirDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirDoyLead.png')

%==========================================================================
%ANALIZO LA DEPENENCIA DEL ERROR EN VELOCIDAD Y DIRECCION CON EL PLAZO DE
%PRONOSTICO Y LA INTENSIDAD DEL VIENTO (PARA EL TOTAL, INVIERNO Y VERANO).
%==========================================================================
MaxVel=25;

for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(:,minlead:maxlead),[1 numel(VelFor(:,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(:,minlead:maxlead),[1 numel(VelObs(:,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(:,minlead:maxlead),[1 numel(DirFor(:,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(:,minlead:maxlead),[1 numel(DirObs(:,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVel(ivel,il) StdErrorVelLeadVel(ivel,il) MeanErrorDirLeadVel(ivel,il) StdErrorDirLeadVel(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVel(ivel,il)=sum(VelIndex);
  
       
 end
 

end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVelSummer(ivel,il) StdErrorVelLeadVelSummer(ivel,il) MeanErrorDirLeadVelSummer(ivel,il) StdErrorDirLeadVelSummer(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVelSummer(ivel,il)=sum(VelIndex);
  
  
       
 end
 

end


%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVelWinter(ivel,il) StdErrorVelLeadVelWinter(ivel,il) MeanErrorDirLeadVelWinter(ivel,il) StdErrorDirLeadVelWinter(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVelWinter(ivel,il)=sum(VelIndex);
         
 end
 
end

MeanErrorVelLeadVel( NLeadVel < MinSampleSize)=NaN;
MeanErrorDirLeadVel( NLeadVel < MinSampleSize)=NaN;
StdErrorVelLeadVel( NLeadVel < MinSampleSize)=NaN;
StdErrorDirLeadVel( NLeadVel < MinSampleSize)=NaN;

MeanErrorVelLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
MeanErrorDirLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
StdErrorVelLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
StdErrorDirLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;

MeanErrorVelLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
MeanErrorDirLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
StdErrorVelLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
StdErrorDirLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;

Vels=(1:MaxVel);
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

%Para la muestra total
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVel);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVel);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVel);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVel);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadVel.png')

%Para el verano
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVelSummer);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVelSummer);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVelSummer);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVelSummer);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadVelSummer.png')

%Para el invierno
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVelWinter);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVelWinter);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVelWinter);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVelWinter);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadVelWinter.png')

%==========================================================================
%ANALIZO LA DEPENENCIA DEL ERROR EN VELOCIDAD Y DIRECCION CON EL PLAZO DE
%PRONOSTICO Y LA DIRECCION DEL VIENTO (PARA EL TOTAL, INVIERNO Y VERANO).
%==========================================================================
MaxDir=360;

for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(:,minlead:maxlead),[1 numel(VelFor(:,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(:,minlead:maxlead),[1 numel(VelObs(:,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(:,minlead:maxlead),[1 numel(DirFor(:,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(:,minlead:maxlead),[1 numel(DirObs(:,minlead:maxlead))]);
        
        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
        
        
  [MeanErrorVelLeadDir(idir,il) StdErrorVelLeadDir(idir,il) MeanErrorDirLeadDir(idir,il) StdErrorDirLeadDir(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDir(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for idir=1:MaxDir

 max_dir=idir + dirres ;
 min_dir=idir - dirres ;
       
 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
   
  [MeanErrorVelLeadDirSummer(idir,il) StdErrorVelLeadDirSummer(idir,il) MeanErrorDirLeadDirSummer(idir,il) StdErrorDirLeadDirSummer(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDirSummer(idir,il)=sum(DirIndex);
  
 end
 

end

%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelFor(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirFor(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);

        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
   
   NLeadDirWinter(idir,il)=sum(DirIndex);

  [MeanErrorVelLeadDirWinter(idir,il) StdErrorVelLeadDirWinter(idir,il) MeanErrorDirLeadDirWinter(idir,il) StdErrorDirLeadDirWinter(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDirWinter(idir,il)=sum(DirIndex);
         
 end
 
end

MeanErrorVelLeadDir( NLeadDir < MinSampleSize)=NaN;
MeanErrorDirLeadDir( NLeadDir < MinSampleSize)=NaN;
StdErrorVelLeadDir( NLeadDir < MinSampleSize)=NaN;
StdErrorDirLeadDir( NLeadDir < MinSampleSize)=NaN;

MeanErrorVelLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
MeanErrorDirLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
StdErrorVelLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
StdErrorDirLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;

MeanErrorVelLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
MeanErrorDirLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
StdErrorVelLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
StdErrorDirLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;

Dirs=(1:MaxDir);
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

%Para la muestra total
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDir);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDir);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDir);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDir);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadDir.png')

%Para el verano
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDirSummer);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDirSummer);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDirSummer);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDirSummer);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadDirSummer.png')

%Para el invierno
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDirWinter);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDirWinter);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDirWinter);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDirWinter);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorForVelDirLeadDirWinter.png')


%==========================================================================
%==========================================================================
%==========================================================================
%ANALIZO LOS ERRORES EN VELOCIDAD Y DIRECCION PARA EL PRONOSTICO CALIBRADO
%==========================================================================
%==========================================================================
%==========================================================================


%==========================================================================
%ANALIZO LA DEPENENCIA CON LA EPOCA DEL ANIO Y EL PLAZO DE PRONOSTICO.
%==========================================================================
  
for idoy=1:365
        
 max_doy=idoy + doyres ;
 min_doy=idoy - doyres ;

 if( max_doy > 365)
   %El tope del perido se paso para el anio siguiente.
   max_doy = max_doy - 365;
   TimeWindowIndex = doy <= max_doy | doy >= min_doy;
 elseif( min_doy < 1)
   min_doy=min_doy + 365;
   TimeWindowIndex = doy >= min_doy | doy <= max_doy;
 else
   TimeWindowIndex = doy >= min_doy & doy <= max_doy;
 end


 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=VelForCalibrated(TimeWindowIndex,minlead:maxlead);
        TmpVelObs=VelObs(TimeWindowIndex,minlead:maxlead);
        TmpDirFor=DirForCalibrated(TimeWindowIndex,minlead:maxlead);
        TmpDirObs=DirObs(TimeWindowIndex,minlead:maxlead);
        
        TmpVelFor=reshape(TmpVelFor,[1 numel(TmpVelFor)]);
        TmpVelObs=reshape(TmpVelObs,[1 numel(TmpVelObs)]);
        TmpDirFor=reshape(TmpDirFor,[1 numel(TmpDirFor)]);
        TmpDirObs=reshape(TmpDirObs,[1 numel(TmpDirObs)]);
          
  [MeanErrorVelDoyLead(idoy,il) StdErrorVelDoyLead(idoy,il) MeanErrorDirDoyLead(idoy,il) StdErrorDirDoyLead(idoy,il) ]=StatisticsVelDirError(TmpVelFor,TmpDirFor,TmpVelObs,TmpDirObs);
  
       
 end
 

end

%Suavizo un poco los patrones de error para visualizar mas claramente la
%dependencia con la epoca del anio y el plazo de pronsotico (hora del dia)
MeanErrorVelDoyLead=SmoothMatrix(MeanErrorVelDoyLead,7,7);
StdErrorVelDoyLead=SmoothMatrix(StdErrorVelDoyLead,7,7);
MeanErrorDirDoyLead=SmoothMatrix(MeanErrorDirDoyLead,7,7);
StdErrorDirDoyLead=SmoothMatrix(StdErrorDirDoyLead,7,7);


horas=(1:NLead)*10/60;
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanErrorVelDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdErrorVelDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,MeanErrorDirDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,doys,StdErrorDirDoyLead);shading flat
ylabel('Dia del anio (dias)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForDirDoyLead.png')

%==========================================================================
%ANALIZO LA DEPENENCIA DEL ERROR EN VELOCIDAD Y DIRECCION CON EL PLAZO DE
%PRONOSTICO Y LA INTENSIDAD DEL VIENTO (PARA EL TOTAL, INVIERNO Y VERANO).
%==========================================================================
MaxVel=25;

for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(:,minlead:maxlead),[1 numel(VelFor(:,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(:,minlead:maxlead),[1 numel(VelObs(:,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(:,minlead:maxlead),[1 numel(DirFor(:,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(:,minlead:maxlead),[1 numel(DirObs(:,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVel(ivel,il) StdErrorVelLeadVel(ivel,il) MeanErrorDirLeadVel(ivel,il) StdErrorDirLeadVel(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVel(ivel,il)=sum(VelIndex);
  
       
 end
 

end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVelSummer(ivel,il) StdErrorVelLeadVelSummer(ivel,il) MeanErrorDirLeadVelSummer(ivel,il) StdErrorDirLeadVelSummer(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVelSummer(ivel,il)=sum(VelIndex);
  
  
       
 end
 

end


%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for ivel=1:MaxVel
        
 max_vel=ivel + velres ;
 min_vel=ivel - velres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        VelIndex= TmpVelFor > min_vel & TmpVelFor < max_vel ;
        
        
  [MeanErrorVelLeadVelWinter(ivel,il) StdErrorVelLeadVelWinter(ivel,il) MeanErrorDirLeadVelWinter(ivel,il) StdErrorDirLeadVelWinter(ivel,il) ]=StatisticsVelDirError(TmpVelFor(VelIndex),TmpDirFor(VelIndex),TmpVelObs(VelIndex),TmpDirObs(VelIndex));
  
  NLeadVelWinter(ivel,il)=sum(VelIndex);
         
 end
 
end

MeanErrorVelLeadVel( NLeadVel < MinSampleSize)=NaN;
MeanErrorDirLeadVel( NLeadVel < MinSampleSize)=NaN;
StdErrorVelLeadVel( NLeadVel < MinSampleSize)=NaN;
StdErrorDirLeadVel( NLeadVel < MinSampleSize)=NaN;

MeanErrorVelLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
MeanErrorDirLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
StdErrorVelLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;
StdErrorDirLeadVelSummer( NLeadVelSummer < MinSampleSize/2)=NaN;

MeanErrorVelLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
MeanErrorDirLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
StdErrorVelLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;
StdErrorDirLeadVelWinter( NLeadVelWinter < MinSampleSize/2)=NaN;

Vels=(1:MaxVel);
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

%Para la muestra total
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVel);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVel);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVel);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVel);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadVel.png')

%Para el verano
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVelSummer);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVelSummer);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVelSummer);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVelSummer);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadVelSummer.png')

%Para el invierno
hFig = figure('Menubar','none') %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20])

subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorVelLeadVelWinter);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorVelLeadVelWinter);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,MeanErrorDirLeadVelWinter);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Vels,StdErrorDirLeadVelWinter);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadVelWinter.png')

%==========================================================================
%ANALIZO LA DEPENENCIA DEL ERROR EN VELOCIDAD Y DIRECCION CON EL PLAZO DE
%PRONOSTICO Y LA DIRECCION DEL VIENTO (PARA EL TOTAL, INVIERNO Y VERANO).
%==========================================================================
MaxDir=360;

for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(:,minlead:maxlead),[1 numel(VelFor(:,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(:,minlead:maxlead),[1 numel(VelObs(:,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(:,minlead:maxlead),[1 numel(DirFor(:,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(:,minlead:maxlead),[1 numel(DirObs(:,minlead:maxlead))]);
        
        DirIndex= TmpDirFor >= min_dir & TmpDirFor <= max_dir ;
        
        
  [MeanErrorVelLeadDir(idir,il) StdErrorVelLeadDir(idir,il) MeanErrorDirLeadDir(idir,il) StdErrorDirLeadDir(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDir(idir,il)=sum(DirIndex);
     
 end
 
end

%Para el verano (Diciembre, Enero,Febrero y Marzo)
 min_doy=334;
 max_doy=90;
 TimeWindowIndex = doy <= max_doy | doy >= min_doy;
for idir=1:MaxDir

 max_dir=idir + dirres ;
 min_dir=idir - dirres ;
       
 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);
        
        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
   
  [MeanErrorVelLeadDirSummer(idir,il) StdErrorVelLeadDirSummer(idir,il) MeanErrorDirLeadDirSummer(idir,il) StdErrorDirLeadDirSummer(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDirSummer(idir,il)=sum(DirIndex);
  
 end
 

end

%Para el invierno (Junio,Julio,Agosto,Septiembre)
 min_doy=151;  
 max_doy=273;
 TimeWindowIndex = doy <= max_doy & doy >= min_doy;
for idir=1:MaxDir
        
 max_dir=idir + dirres ;
 min_dir=idir - dirres ;

 for il=1:NLead
    
    minlead=il-leadres;
    maxlead=il+leadres;
    
    if( minlead < 1);minlead=1;end
    if( maxlead > NLead);maxlead=NLead;end
        
        TmpVelFor=reshape(VelForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(VelFor(TimeWindowIndex,minlead:maxlead))]);
        TmpVelObs=reshape(VelObs(TimeWindowIndex,minlead:maxlead),[1 numel(VelObs(TimeWindowIndex,minlead:maxlead))]);
        TmpDirFor=reshape(DirForCalibrated(TimeWindowIndex,minlead:maxlead),[1 numel(DirFor(TimeWindowIndex,minlead:maxlead))]);
        TmpDirObs=reshape(DirObs(TimeWindowIndex,minlead:maxlead),[1 numel(DirObs(TimeWindowIndex,minlead:maxlead))]);

        DirIndex= TmpDirFor > min_dir & TmpDirFor < max_dir ;
   
   NLeadDirWinter(idir,il)=sum(DirIndex);

  [MeanErrorVelLeadDirWinter(idir,il) StdErrorVelLeadDirWinter(idir,il) MeanErrorDirLeadDirWinter(idir,il) StdErrorDirLeadDirWinter(idir,il) ]=StatisticsVelDirError(TmpVelFor(DirIndex),TmpDirFor(DirIndex),TmpVelObs(DirIndex),TmpDirObs(DirIndex));
  
  NLeadDirWinter(idir,il)=sum(DirIndex);
         
 end
 
end

MeanErrorVelLeadDir( NLeadDir < MinSampleSize)=NaN;
MeanErrorDirLeadDir( NLeadDir < MinSampleSize)=NaN;
StdErrorVelLeadDir( NLeadDir < MinSampleSize)=NaN;
StdErrorDirLeadDir( NLeadDir < MinSampleSize)=NaN;

MeanErrorVelLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
MeanErrorDirLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
StdErrorVelLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;
StdErrorDirLeadDirSummer( NLeadDirSummer < MinSampleSize/2)=NaN;

MeanErrorVelLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
MeanErrorDirLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
StdErrorVelLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;
StdErrorDirLeadDirWinter( NLeadDirWinter < MinSampleSize/2)=NaN;

Dirs=(1:MaxDir);
doys=1:365;
fontsize=12;

%Grafico la dependencia de los errores y su desviacion estandard con la
%hora el plazo de pronsotico y la epoca del anio.

%Para la muestra total
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDir);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDir);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDir);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDir);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadDir.png')

%Para el verano
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDirSummer);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDirSummer);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDirSummer);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDirSummer);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadDirSummer.png')

%Para el invierno
figure
subplot(2,2,1)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorVelLeadDirWinter);shading flat
caxis([-3 1])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en velocidad')
colorbar

subplot(2,2,2)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorVelLeadDirWinter);shading flat
caxis([0 3])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en velocidad')
colorbar

subplot(2,2,3)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,MeanErrorDirLeadDirWinter);shading flat
caxis([-15 15])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Error medio en direccion')
colorbar

subplot(2,2,4)
set(gca,'FontSize',fontsize);

pcolor(horas,Dirs,StdErrorDirLeadDirWinter);shading flat
caxis([0 90])
ylabel('Velocidad (m/s)');
xlabel('Plazo de pronostico (horas)');
title('Desvio estandard del error en direccion')
colorbar

print('-dpng','ErrorCalibratedForVelDirLeadDirWinter.png')

 
 
 
 
 