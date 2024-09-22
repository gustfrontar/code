function group=group_error_spread_fun_2(group)

%Esta funcion recibe una serie de grupos y calcula el error en la posicion
%de la media del ensamble. El spread del ensamble en la posicion. El rank
%de la observacion en lat y lon y eventualmente en otras variables. Y el
%error ideal (un error aleatorio tomado a partir de considerar que alguno
%de los miembros del ensamble es la verdad y el resto constituyen el
%ensamble).


%Asumimos que el primer miembro del ensamble es el miembro control.

ngroup=size(group.minlat,3);
flength=size(group.minlat,2);
enssize=size(group.minlat,1);


meandisterror=NaN(size(group.minlatanalysis));
ctrldisterror=NaN(size(group.minlatanalysis));
idealdisterror=NaN(size(group.minlatanalysis));
distspread=NaN(size(group.minlatanalysis));
meandistlaterror=NaN(size(group.minlatanalysis));
meandistlonerror=NaN(size(group.minlatanalysis));
ctrldistlaterror=NaN(size(group.minlatanalysis));
ctrldistlonerror=NaN(size(group.minlatanalysis));
distlatspread=NaN(size(group.minlatanalysis));
distlonspread=NaN(size(group.minlatanalysis));
distlatpit=NaN(size(group.minlatanalysis));
distlonpit=NaN(size(group.minlatanalysis));


   %Calculo cuantos miembros del ensamble tengo para cada tiempo. (esto me
   %va a servir luego para filtrar los resultados si fuera necesario).
   
   nummembers=squeeze(sum(~isnan(group.minlat),1));

   %Calculo la probabilidad dada por el ensamble de la existencia del
   %sistema.
   sistemprobability=nummembers/size(group.minlat,1);
   sistemexistance=squeeze(~isnan(group.minlatanalysis));

for ii=1:ngroup
 
   %Calculo el error en posicion de la media del ensamble y del control 
   for jj=1:flength
       if( ~isnan(group.meanlat(jj,ii)) && ~isnan(group.minlatanalysis(jj,ii)))
         meandisterror(jj,ii)=distll_fun(group.meanlon(jj,ii),group.meanlat(jj,ii),group.minlonanalysis(jj,ii),group.minlatanalysis(jj,ii));
       end
       if( ~isnan(group.minlat(1,jj,ii)) && ~isnan(group.minlatanalysis(jj,ii)))
         ctrldisterror(jj,ii)=distll_fun(group.minlon(1,jj,ii),group.minlat(1,jj,ii),group.minlonanalysis(jj,ii),group.minlatanalysis(jj,ii));
       end
   end
   
   %Calculo el error ideal, este seria el error que existiria si uno de los
   %miembros del ensamble fuera la verificacion. 
   
   %Para cada grupo voy a usar un miembro del ensamble diferente. Genero un numero aleatorio entre
   %1 y el numero de miembros del ensamble.
   auxvector=0:1/enssize:1;
   truemember=sum(rand > auxvector);

   tmplat=group.minlat(:,:,ii);
   tmplon=group.minlon(:,:,ii);
   truelat=tmplat(truemember,:);
   truelon=tmplon(truemember,:);
   %Eliminamos al "truemember" del ensamble, ahora es la "verdad".
   tmplat(truemember,:)=NaN;
   tmplon(truemember,:)=NaN;
   %Recalculo la media del grupo.
   tmpmeanlat=squeeze(nanmean(tmplat,1));
   tmpmeanlon=NaN(size(tmpmeanlat));
   for kt=1:flength %Cuidado especial hay que tener siempre que se promedian longitudes.
   tmpmeanlon(kt)=mean_lon_fun(tmplon(:,kt));
   end
   for jj=1:flength
     %Ahora si calculo el error en distancia entre ambas.
     if( ~isnan(tmpmeanlat(jj)) && ~isnan(truelat(jj)))
          idealdisterror(jj,ii)=distll_fun(tmpmeanlon(jj),tmpmeanlat(jj),truelon(jj),truelat(jj));
     end
   end
   
   %Calculamos el spread en distancia
   tmplat=group.minlat(:,:,ii);
   tmplon=group.minlon(:,:,ii);
   tmpdist=NaN(size(tmplat));
   tmpmeanlat=nanmean(tmplat,1);
   tmpmeanlon=NaN(size(tmpmeanlat));
   for kt=1:flength %Cuidado especial hay que tener siempre que se promedian longitudes.
   tmpmeanlon(kt)=mean_lon_fun(tmplon(:,kt));
   end
   
   %Compute between each ensemble member and the mean.
   for jj=1:flength
       for kk=1:enssize
           if( ~isnan(tmplat(kk,jj)) )
              tmpdist(kk,jj)=distll_fun(tmplon(kk,jj),tmplat(kk,jj),tmpmeanlon(jj),tmpmeanlat(jj));
           end
       end
   end
   %Compute the spread in the distance. (Para el caso particular de la
   %distancia, no se debe calcular el std de las distancias, sino la
   %distancia media entre cada uno de los miembros y la media del
   %ensamble).
   distspread(:,ii)=nanmean(tmpdist,1);
   
   
   %Calculamos el error en la longitud y el la latitud independientemente.
   %Para compatibilizar los errores (particularmente el de la longitud)
   %asignamos una distancia a cada error basados en la latitud a que se
   %encuentran.
   
   %La cuenta siguiente resulta en NaN si el pronostico o la observacion
   %son NaN.
   latfactor=111000;
   meandistlaterror(:,ii)=(group.meanlat(:,ii)-group.minlatanalysis(:,ii))*latfactor;
   ctrldistlaterror(:,ii)=(squeeze(group.minlat(1,:,ii))'-group.minlatanalysis(:,ii))*latfactor;
   distlatspread(:,ii)=nanstd(group.minlat(:,:,ii),[],1)*latfactor;
   

   tmplat=(group.meanlat(:,ii)+group.minlatanalysis(:,ii))/2;
   meandistlonerror(:,ii)=diff_lon_fun(squeeze(group.meanlon(:,ii)),group.minlonanalysis(:,ii))*latfactor.*cosd(tmplat);
   tmplat=(group.minlat(1,:,ii)'+group.minlatanalysis(:,ii))/2;
   ctrldistlonerror(:,ii)=diff_lon_fun(group.minlon(1,:,ii)',group.minlonanalysis(:,ii))*latfactor.*cosd(tmplat);
   for kk=1:enssize
    tmplat=(group.minlat(kk,:,ii)+group.minlatanalysis(:,ii)')/2;
    tmp(kk,:)=diff_lon_fun(group.minlon(kk,:,ii),group.minlonanalysis(:,ii))*latfactor.*cosd(tmplat);
   end
   distlonspread(:,ii)=nanstd(tmp,[],1);
   
   %Calculamos el rank para la distancia en la direccion de las longitudes
   %y de las latitudes.
   for jj=1:flength
     tmppit=sum(~isnan(group.minlat(:,jj,ii)));
      if(tmppit > 1)
        distlatpit(jj,ii)=sum(group.minlat(:,jj,ii) > group.minlatanalysis(jj,ii))/tmppit;
        distlonpit(jj,ii)=(rank_lon_fun(group.minlon(:,jj,ii),group.minlonanalysis(jj,ii))-1)/tmppit;
      end
   
   end
    
   %TODO CALCULAR ERROR DE LA MEDIA, DEL CONTROL, SPREAD, SPREAD IDEAL,
   %RANK PARA OTRAS PROPIEDADES DEL SISTEMA (INTENSIDAD TAMANIO Y
   %VELOCIDAD).
      
end

%CALCULO DISPERSION Y ERROR PARA OTRAS VARIABLES.

group.meananomerror=(squeeze(nanmean(group.minanom,1))-group.minanomanalysis);
group.meananomspread=(squeeze(nanstd(group.minanom)));
tmp=NaN(enssize,flength,ngroup);
for i=1:enssize
   tmp(i,:,:)=group.minanomanalysis; 
end
group.meananomrank=sum(tmp > group.minanom,1)+1;

group.meanlaperror=(squeeze(nanmean(group.minlap,1))-group.minlapanalysis);
group.meanlapspread=(squeeze(nanstd(group.minlap)));
tmp=NaN(enssize,flength,ngroup);

for i=1:enssize
   tmp(i,:,:)=group.minlapanalysis; 
end
group.meanlaprank=sum(tmp > group.minlap,1)+1;

%CALCULO LA OBSERVACION NORMALIZADA POR LA MEDIA DEL ENSAMBLE Y SU
%DISPERSION.


group.rcvlon=meandistlonerror./distlonspread;
group.rcvlat=meandistlaterror./distlatspread;


group.nummembers=nummembers;
group.meandisterror=meandisterror;
group.ctrldisterror=ctrldisterror;
group.idealdisterror=idealdisterror;
group.distspread=distspread;
group.meandistlaterror=meandistlaterror;
group.meandistlonerror=meandistlonerror;
group.ctrldistlaterror=ctrldistlaterror;
group.ctrldistlonerror=ctrldistlonerror;
group.distlatspread=distlatspread;
group.distlonspread=distlonspread;
group.distlatpit=distlatpit;
group.distlonpit=distlonpit;
group.sistemprobability=sistemprobability;
group.sistemexistance=sistemexistance;

