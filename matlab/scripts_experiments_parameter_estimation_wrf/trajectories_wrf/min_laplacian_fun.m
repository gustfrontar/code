function [ minstruct MASCARA_SIS ] = min_laplacian_fun( FIELD,LON,LAT,UMB_TB,UMB_SIS,UMBRAL_ASOCIACION,UMBRAL_CERCANIA )



    [ny nx]=size(FIELD);
    final_resol=0.5;            %Resolucion a la que se va a interpolar localmente el campo usando los splines.

    NMINIMOS=0;
    MIN_I=0;
    MIN_J=0;

    MASCARA_MIN=zeros(size(FIELD));

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
    % Armamos la estructura con las salidas
    %=========================================================================%

    for ii=1:NMINIMOS

        minstruct.id(ii)=ii;

    end

    minstruct.minlat=MIN_LAT;
    minstruct.minlon=MIN_LON;
    minstruct.nminimos=NMINIMOS;
    minstruct.minval=MIN_TMP;
    minstruct.mini=MIN_I;
    minstruct.minj=MIN_J;

    %=========================================================================%
    % CALCULO DE LAS CUENCAS ASOCIADAS A CADA MINIMO.
    %=========================================================================%
     MASCARA_SIS=NaN(size(FIELD));

    for j=1:nx

        for i=2:ny-1

          if( FIELD(i,j) <= UMB_SIS )

              [minimun]=minimun_basin_fun(MASCARA_MIN,FIELD,i,j);
              MASCARA_SIS(i,j)=minimun;

          end

        end

    end

    %=========================================================================%
    % ANTES DE TERMINAR VERIFICO SI HAY ASOCIACION ENTRE LOS MINIMOS 
    %=========================================================================%   

    %for iii=1:6

        [minstruct MASCARA_SIS]=link_min(FIELD,MASCARA_SIS,minstruct,UMBRAL_ASOCIACION,UMBRAL_CERCANIA);

    %end

end

%=========================================================================%
