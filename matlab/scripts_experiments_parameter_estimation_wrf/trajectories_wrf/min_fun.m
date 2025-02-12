function [ minstruct MASCARA_SIS ] = min_fun( FIELD,LON,LAT,UMB_TB,UMB_SIS,UMBRAL_ASOCIACION,UMBRAL_CERCANIA )

    %UNTITLED2 Summary of this function goes here
    %   Detailed explanation goes here

    [ny nx]=size(FIELD);

    NMINIMOS=0;
    MIN_I=0;
    MIN_J=0;

    MASCARA_MIN=zeros(size(FIELD));
    MASCARA_SIS=zeros(size(FIELD));

    %=========================================================================%

    for j=1:nx

        for i=2:ny-1

           if( FIELD(i,j) < UMB_TB)

               testmin=0;

               for ii=-1:1

                   for jj=-1:1

                           indexi=i+ii;
                           indexj=j+jj;

                        if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 && ~(indexi==i && indexj==j))

                           if(FIELD(indexi,indexj) <= FIELD(i,j) )

                               testmin=1;

                           end

                        end

                   end

               end

               if(testmin==0)

               NMINIMOS=NMINIMOS+1;
               MIN_TMP(NMINIMOS)=FIELD(i,j);
               MIN_LAT(NMINIMOS)=LAT(i,j);
               MIN_LON(NMINIMOS)=LON(i,j);
               MIN_I(NMINIMOS)=i;
               MIN_J(NMINIMOS)=j;
               MASCARA_MIN(i,j)=NMINIMOS;

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
    
    %=========================================================================%
    % Suavizo la posicion de los minimos usando informacion de los 8 puntos
    % que rodean a cada minimo.
    %=========================================================================%  
    
    
 for ii=1:NMINIMOS


    %Vamos a calcular un minimo aproximado usando la informacion de los
    %puntos vecinos. Uso un promedio pesado por la anomalia (donde la
    %anomalia es mayor voy a obtener un mayor peso y por ende la media de
    %las posiciones va a estar sesgada hacia el sector donde la anomalia es
    %mayor. Voy a promediar la latitud del punto en cuestion y la de los
    %puntos adyacentes.



    if( ~(MIN_I(ii) <= 2 || MIN_I(ii) >= ny-1 || MIN_J(ii) <= 2 || MIN_J(ii) >= ny-1 ) )
        %No aplico este algoritmo en los bordes

    tmplat=NaN(5,5);
    tmplon=NaN(5,5);
    tmpdat=NaN(5,5);

        for jj=-2:2
            index=MIN_J(ii)+jj;
%             if(index < 1)
%                 index=nx;
%             elseif(index > nx)
%                 index=1;
%             end
            tmpi=MIN_I(ii)-2:MIN_I(ii)+2;
            tmplat(:,jj+3)=LAT(tmpi,index);
            tmplon(:,jj+3)=LON(tmpi,index);
            tmpdat(:,jj+3)=(UMB_SIS-FIELD(tmpi,index)).^2; 



        end

    %Calculamos la media en latitudes
    MIN_LAT(ii)=mean(tmplat(:).*tmpdat(:))/mean(tmpdat(:));
    %Calculamos la media en longitudes teniendo en cuenta todos los
    %posibles casos.
    if(max(tmplon(:))-min(tmplon(:)) > 180)
       if(max(tmplon(:)) > 180)
           tmplon(tmplon > 180)=tmplon(tmplon > 180)-360;
           MIN_LON(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
           if(MIN_LON(ii) < 0)
               MIN_LON(ii)=MIN_LON(ii)+360;
           end
       elseif(min(tmplon(:)) < 0)
         tmplon(tmplon < 0)=tmplon(tmplon < 0)+360;
         MIN_LON(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
         if(MIN_LON(ii) > 180)
             MIN_LON(ii)=MIN_LON(ii)-360;
         end
       end
    else
    MIN_LON(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
    end

    %En esta parte tambien se puede usar una interpolacion por splines
    %bidimensionales como en el programa de Manuel. 
    end


end

    
    

    %=========================================================================%
    % Armamos la estructura con las salidas
    %=========================================================================%

    for ii=1:NMINIMOS

        minstruct.id(ii)=ii;

    end

    if( NMINIMOS > 0)
      minstruct.minlat=MIN_LAT;
      minstruct.minlon=MIN_LON;
      minstruct.nminimos=NMINIMOS;
      minstruct.minval=MIN_TMP;
      minstruct.mini=MIN_I;
      minstruct.minj=MIN_J;
    else
      minstruct.id=[];
      minstruct.minlat=[];
      minstruct.minlon=[];
      minstruct.nminimos=0;
      minstruct.minval=[];
      minstruct.mini=[];
      minstruct.minj=[]; 
      minstruct.remove=[];
    end

    %=========================================================================%
    % CALCULO DE LAS CUENCAS ASOCIADAS A CADA MINIMO.
    %=========================================================================%
    if( NMINIMOS > 0)

    for j=1:nx

        for i=2:ny-1

          if( FIELD(i,j) <= UMB_SIS )

              [minimun]=minimun_basin_fun(MASCARA_MIN,FIELD,i,j);
              MASCARA_SIS(i,j)=minimun;

          end

        end

    end
    end

    %=========================================================================%
    % ANTES DE TERMINAR VERIFICO SI HAY ASOCIACION ENTRE LOS MINIMOS 
    %=========================================================================%   
    if( NMINIMOS > 0)
    for iii=1:3

        [minstruct MASCARA_SIS]=link_min(FIELD,MASCARA_SIS,minstruct,UMBRAL_ASOCIACION,UMBRAL_CERCANIA,UMB_TB);

    end
    end
    
    

end

%=========================================================================%
