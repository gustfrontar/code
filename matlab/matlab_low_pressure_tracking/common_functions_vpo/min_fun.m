function [ minstruct ] = min_fun( FIELD,LAPLACIAN,LON,LAT,YREV)
%ESTA FUNCION CALCULA LOS MINIMOS LOCALES EN EL CAMPO DE GEOPOTENCIAL Y USA
%SPLINES CUBICOS PARA DETERMINAR SU POSICION EN UNA ESCALA MENOR.

[ny nx]=size(FIELD);
final_resol=0.5;            %Resolucion a la que se va a interpolar localmente el campo usando los splines.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DETECCION DE LOS MINIMOS Y LA INTERPOLACION CON SPLINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NMINIMOS=0;
MIN_I=0;
MIN_J=0;
UMB_INTENSIDAD=0;
UMB_ANOMALY=0;           
%UMBRAL_AREA=20;          %CUANTOS MGP VAMOS A CONSIDERAR ENTRE EL MINIMO Y UN PUNTO CUALQUIERA PARA ESTIMAR EL AREA.
UMB_AREA_2=-50;           %VAMOS A CONSIDERAR QUE UN PUNTO FORMA PARTE DE UN SISTEMA SI SU ANOMALIA DE GEOPOTENCIAL ES MENOR QUE ESTE UMBRAL.
%pcolor(FIELD)

for j=1:nx
    for i=2:ny-1

       if(LAPLACIAN(i,j) < UMB_INTENSIDAD && FIELD(i,j) < UMB_ANOMALY)
       testmin=0;
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   if(FIELD(indexi,indexj) < FIELD(i,j) )
                   testmin=1;
                   end

           end
       end
       

       if(testmin==0)


       %Para encontrar la posicion del minimo interpolo el campo usando
       %splines cubicos.
       %Primero genero una reticula alrededor del minimo.
       LOCALFIELD=NaN(3,3);
       LOCALLAT  =NaN(3,3);
       LOCALLON  =NaN(3,3);
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   LOCALFIELD(ii+2,jj+2)=FIELD(indexi,indexj);
                   
                   LOCALLAT(ii+2,jj+2)  =LAT(indexi,indexj);
                   if( i==1 && ii==-1)
                   LOCALLAT(ii+2,jj+2)=LAT(1,indexj)+original_resol;    
                   end
                   if( i==ny && ii==1)
                   LOCALLAT(ii+2,jj+2)=LAT(ny,indexj)-original_resol;
                   end
                   LOCALLON(ii+2,jj+2)  =LON(indexi,indexj);
                   if( j == 1 && jj==-1)
                   LOCALLON(ii+2,jj+2)=LON(indexi,indexj)-360;
                   end
                   if( j == nx && jj==1)
                   LOCALLON(ii+2,jj+2)=LON(indexi,indexj)+360;
                   end
           end
       end
       if(YREV)
       LOCALLATHIRES=LOCALLAT(1,1):-final_resol:LOCALLAT(3,1);
       else
       LOCALLATHIRES=LOCALLAT(1,1):final_resol:LOCALLAT(3,1);
       end
       LOCALLONHIRES=LOCALLON(1,1):final_resol:LOCALLON(1,3);
       [LOCALLONHIRES LOCALLATHIRES] = meshgrid(LOCALLONHIRES,LOCALLATHIRES);

       LOCALFIELDHIRES=interp2(LOCALLON,LOCALLAT,LOCALFIELD,LOCALLONHIRES,LOCALLATHIRES,'splines');
       
       
       [localny localnx]=size(LOCALFIELDHIRES);
       LOCALFIELDHIRES=reshape(LOCALFIELDHIRES,[1 localny*localnx]);
       LOCALLONHIRES=reshape(LOCALLONHIRES,[1 localny*localnx]);
       LOCALLATHIRES=reshape(LOCALLATHIRES,[1 localny*localnx]);
       [minimo minpos]=min(LOCALFIELDHIRES);
       
       NMINIMOS=NMINIMOS+1;
       MINLAP(NMINIMOS)=LAPLACIAN(i,j);
       MIN_ANOM(NMINIMOS)=minimo;
       MIN_LAT(NMINIMOS)=LOCALLATHIRES(minpos);
       MIN_LON(NMINIMOS)=LOCALLONHIRES(minpos);
       MIN_I(NMINIMOS)=i;
       MIN_J(NMINIMOS)=j;
       if(MIN_LON(NMINIMOS) < 0)
           MIN_LON(NMINIMOS)=MIN_LON(NMINIMOS)+360;
       end
       if(MIN_LON(NMINIMOS) > 360)
           MIN_LON(NMINIMOS)=MIN_LON(NMINIMOS)-360;
       end

       end

       end
    end
end

%Vamos a testear los minimos en el polo.
testmin=0;
for ii=1:nx
   if(FIELD(1,1) > FIELD(2,ii))
   testmin=1;
   end
end
if(testmin==0)
   NMINIMOS=NMINIMOS+1;
   MIN_I(NMINIMOS)=1;
   MIN_J(NMINIMOS)=1;
   MINLAP(NMINIMOS)=LAPLACIAN(1,1);
   MIN_LAT(NMINIMOS)=LAT(1,1);
   MIN_LON(NMINIMOS)=LON(1,1);
   MIN_ANOM(NMINIMOS)=FIELD(1,1);
end
testmin=0;
for ii=1:nx
   if(FIELD(ny,1) > FIELD(ny-1,ii))
   testmin=1;
   end
end
if(testmin==0)
   NMINIMOS=NMINIMOS+1;
   MIN_I(NMINIMOS)=ny;
   MIN_J(NMINIMOS)=1;
   MINLAP(NMINIMOS)=LAPLACIAN(ny,1);
   MIN_LAT(NMINIMOS)=LAT(ny,1);
   MIN_LON(NMINIMOS)=LON(ny,1);
   MIN_ANOM(NMINIMOS)=FIELD(ny,1);

end

for ii=1:NMINIMOS
MIN_ID(ii)=ii;
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIN DE LA DETECCION DE LOS MINIMOS Y LA INTERPOLACION CON SPLINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFICACION DE LOS SISTEMAS Y CALCULO DEL AREA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

UNDEF=-99;

MASCARA_MINIMOS=zeros(ny,nx);
for imin=1:NMINIMOS
   MASCARA_MINIMOS(MIN_I(imin),MIN_J(imin))=imin;
   if(MIN_I(imin)==ny || MIN_I(imin)==1)
       MASCARA_MINIMOS(MIN_I(imin),:)=imin;
   end
end
MASCARA=MASCARA_MINIMOS;
  
    [TMPMINPN TMPMINLOCPN]=min(FIELD(ny-1,:));
    [TMPMINPS TMPMINLOCPS]=min(FIELD(2,:));
     
%NOW WE WILL DO A LOOP OVER ALL GRID POINTS AND IF THE FIELD VALUE IS UNDER THE
%THRESHOLD THEN WE WILL FOLLOW THE FASTEST DESCENDING PATH UNTIL WE REACH A MINIMUN
%THAT WILL BE THE ASSOCIATED MINIMUN TO THIS GRID POINT.


for j=1:nx
  for i=1:ny

    if(FIELD(i,j) < UMB_AREA_2 && MASCARA_MINIMOS(i,j) == 0)
    %WE WILL NOT THAKE INTO ACCOUNT THE POSITION OF THE MINIMUNS THIS POINTS WILL REMAIN UNCHANGED

         MAXITER=round(nx/2.0);
       
         %THE PATH STARST AT I and J, THEN PATHI AND PATHJ STORES THE SUBSECUENTS STEPS.
         pi=i;
         pj=j;

         PATH_MASK=true(ny,nx);  %THIS MASK CONTROLS THAT THE PATH DOES NOT GO BACK.
  
         PATH_MASK(pi,pj)=false;
         
         CONT=true;       %CONTROLS THE ITERATION, WHILE TRUE THE PATH WILL CONTINUE UNTIL A MINIMUN IS
                          %REACHED OR UNTIL THE MAXIMUN NUMBER OF ITERATIONS IS REACHED.
  
         CONTADOR=1;
         TMP_MIN=FIELD(pi,pj);
  
      while(CONT)
  
      %I will search in all directions which is the one corresponding to
      %the maximun descent.
      %First we check if we are at the pole because this is a particular case.
      if(pi == ny)
       %I assume that different j at ny have the same value and that they represent
       %the field value at the north pole.
       TMP_MIN=TMPMINPN;
       pj=TMPMINLOCPN;
       pi=ny-1;
       PATH_MASK(ny,:)=false;

      elseif(pi == 1) %REPEAT FOR THE SOUTH POLE
       pi=2;
       TMP_MIN=TMPMINPS;
       pj=TMPMINLOCPS;
       PATH_MASK(1,:)=false;
       
      else
       %We are not at the pole, so I have to check all neighbours to see were is the direction of maximun descent. 
       pi_o=pi;
       pj_o=pj;

         for ii=-1:1
           for jj=-1:1
                %First we check that we are not evaluating pi and pj.
                if( ~( ii == 0 && jj == 0 ))
                   indexi=pi_o+ii;
                   indexj=pj_o+jj;
                   %USE GLOBAL BOUNDARY CONDITIONS 
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);

                if(FIELD(indexi,indexj) <= TMP_MIN && PATH_MASK(indexi,indexj)) 
                TMP_MIN=FIELD(indexi,indexj); 
                pi=indexi;
                pj=indexj;

                end
                end
           end
         end
         
   
      end %Este es el end del if sobre si se trata de un punto del polo o de uno comun.
          %Keep counting how many steps has been performed for the current path.
           CONTADOR=CONTADOR+1;
           PATH_MASK(pi,pj)=false; %Change the mask value for pi,pj so this point cannot be used again for the same path.
           
           %Test if we have reached a minimun (or a point already belonging
           %to a minimum.
           if( MASCARA_MINIMOS(pi,pj) > 0 )
               %if(FIELD(i,j)-FIELD(pi,pj) < UMBRAL_AREA)
               MASCARA(i,j)=MASCARA_MINIMOS(pi,pj);
               %end
               CONT=false;
           end
           %Test if we haven't reached the maximun path length.
           if(CONTADOR > MAXITER)
               CONT=false;
               MASCARA(i,j)=UNDEF;
           end
               
      end %END WHILE

    end  %END DEL IF SOBRE SI LA ANOMALIA ESTA POR DEBAJO DEL UMBRAL.
       
  end %END OF j loop
end  %END OF i loop 

%Compute system area.
DELTA_LAT(2:ny,1:nx)=LAT(2:ny,:)-LAT(1:ny-1,:);
DELTA_LON(1:ny,2:nx)=LON(:,2:nx)-LON(:,1:nx-1);

DELTA_LON(:,1)=LON(:,2)-LON(:,1);
DELTA_LAT(1,:)=LAT(2,:)-LAT(1,:);

%CORRECT FOR THE DISCONTINUITY IN LONGITUDE.
DELTA_LON(DELTA_LON > 360)=DELTA_LON(DELTA_LON > 360)-360;
PI=acos(-1.);

%COMPUTE THE BOX SIZE ASSOCIATED WITH EACH GRID POINT
BOX_SIZE=(111000.^2).*(DELTA_LON.*cos(LAT.*PI/180).*DELTA_LAT);
   

%START A LOOP OVER THE DIFFERENT SYSTEMS TO GET SYSTEM PROPERTIES.
MIN_GRID_SIZE=0;
for ii=1:NMINIMOS

    MINMASK=(MASCARA==ii);

    %COMPUTE SYSTEM AREA.
    AREA(ii)=sum(sum(BOX_SIZE(MINMASK)));
    %COMPUTE MEAN SYSTEM ANOMALY
    MEAN_ANOM(ii)=mean(FIELD(MINMASK));
    
%LO QUE VIENE A CONTINUACION QUEDO DE UNA IMPLEMENTACION ANTERIOR QUE PUEDE SER 
%INTERESANTE EN EL FUTURO PARA AGREGAR NUEVAS VARIABLES A LA VERIFICACION
% VIENTO, VORTICIDAD, DIVERGENCIA,.... 

%    %COMPUTE MEAN, MIN AND MAX FOR ALL THE AVAILABLE ADDITIONAL DATA.
%
%    IF(READ_ADDITIONAL_DATA)THEN
%      DO IVAR=1,NF
%        MIN_ADDITIONAL_DATA(ii,IVAR)=MINVAL(ADDITIONALFIELD(:,:,IVAR),MASK)
%        MAX_ADDITIONAL_DATA(ii,IVAR)=MAXVAL(ADDITIONALFIELD(:,:,IVAR),MASK)
%        MEAN_ADDITIONAL_DATA(ii,IVAR)=SUM(ADDITIONALFIELD(:,:,IVAR),MASK)/REAL(COUNT(MASK))
%      ENDDO
%    ENDIF

     
end  %END DO OVER THE SYSTEMS.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIN DE LA IDENTIFICACION DE LOS SISTEMAS Y CALCULO DEL AREA.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minstruct.minlat=MIN_LAT;
minstruct.minlon=MIN_LON;
minstruct.minlap=MINLAP;
minstruct.minarea=AREA;
minstruct.meananom=MEAN_ANOM;
minstruct.nminimos=NMINIMOS;
minstruct.minanom=MIN_ANOM;
minstruct.mini=MIN_I;
minstruct.minj=MIN_J;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTEO DE ASOCIACION ENTRE MINIMOS (DETECCION DE MINIMOS CERCANOS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ANTES DE TERMINAR VERIFICO SI HAY ASOCIACION ENTRE MINIMOS.
[minstruct]=link_min(FIELD,minstruct);

% figure
% pcolor(MASCARA)
% 
% figure
% pcolor(MASCARA_MINIMOS)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIN DEL TESTEO DE LA ASOCIACION ENTRE MINIMOS.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


end


