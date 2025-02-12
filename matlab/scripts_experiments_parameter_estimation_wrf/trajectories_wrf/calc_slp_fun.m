  function  [SLP]= calc_slp_fun(P,Z,T,QV)
  %Function to compute SLP.
  %Taken from ARWpost module module_calc_slp
  %P in pascals
  %Z in meters
  %T in Kelvin
  %QV in kg/kg
  %Array order is (times,z,x,y)
  
  %Constants ....
  G = 9.81;
  Rd = 287.04;
  TC=273.16+17.5;
  GAMMA = 0.0065;   %Standard gradient
  PCONST = 10000.;  %100 hPa.



  [times bottom_top_dim west_east_dim south_north_dim]=size(P);

  for it=1 : times

%     Find least zeta level that is PCONST Pa above the surface.  We later use this
%     level to extrapolate a surface pressure and temperature, which is supposed
%     to reduce the effect of the diurnal heating cycle in the pressure field.


  for j = 1 : south_north_dim
    for i = 1 : west_east_dim

      level(i,j) = -1;
      k = 1;
      found = false;
      while( (~ found) & (k <= bottom_top_dim) )
        if ( P(it,k,i,j) < P(it,1,i,j)-PCONST ) 
          level(i,j) = k;
          found = true;
        end
        k = k+1;
      end

    end
  end


% Get temperature PCONST Pa above surface.  Use this to extrapolate
% the temperature at the surface and down to sea level.

  for j = 1 : south_north_dim
    for i = 1 : west_east_dim

      klo = max([level(i,j) - 1  1]);
      khi = min([klo + 1 bottom_top_dim - 1]);

      if( klo == khi )
        error=1;
      end

      plo = P(it,klo,i,j);
      phi = P(it,khi,i,j);
      tlo = T(it,klo,i,j)*(1. + 0.608 * QV(it,klo,i,j) );
      thi = T(it,khi,i,j)*(1. + 0.608 * QV(it,khi,i,j) );
      zlo = Z(it,klo,i,j);
      zhi = Z(it,khi,i,j);

      p_at_pconst = P(it,1,i,j) - PCONST;
      t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi);
      z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi);

      t_surf(i,j) = t_at_pconst*(P(it,1,i,j)/p_at_pconst)^(GAMMA*Rd/G);
      t_sea_level(i,j) = t_at_pconst+GAMMA*z_at_pconst;

    end
  end


%     If we follow a traditional computation, there is a correction to the sea level
%     temperature if both the surface and sea level temperatures are *too* hot.

%  IF ( traditional_comp ) THEN
    for  j = 1 : south_north_dim;
      for i = 1 : west_east_dim;
        l1 = t_sea_level(i,j) < TC;
        l2 = t_surf     (i,j) <= TC;
        l3 = ~ l1;
         if( l2 & l3 ) 
          t_sea_level(i,j) = TC;
         else
          t_sea_level(i,j) = TC - 0.005*(t_surf(i,j)-TC)^2;
         end
      end
    end
%  END IF


%     The grand finale

  %for j = 1 : south_north_dim
  %  for i = 1 : west_east_dim
      z_half_lowest=squeeze(Z(it,1,:,:));
      SLP(:,:,it) = 0.01*squeeze(P(it,1,:,:)) .*          ...
                            exp((2.*G*z_half_lowest)./   ...
                            (Rd*(t_sea_level+t_surf)));

  %  end
  %end


end %End of the loop over the times.


