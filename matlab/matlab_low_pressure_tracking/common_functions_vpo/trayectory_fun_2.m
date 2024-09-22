
function [trajectory]=trayectory_fun_2(config,MinStruct)
%This function associates minimuns at different times to compute the
%trajectory of the different systems.

trajectory=[];

MaxPropVel=35;                                       %Velocidad maxima de propagacion en m/s.
MaxTravelDistance=MaxPropVel*config.timefrec*3600;   %Maximun distance that a system can travel between two frames.
MaxCostFunction=0.5; %0.15 es el valor por default para el typo1, 0.5 es el default para el typo2.
CostFunctionType=2;   %1=continuidad en dir y modulo de V, 2=magnitud de cambio en V.
  
prematurebreak=true;
reconection=true;
redevelopment=true;
trajsurgery=false; %ESTA OPCION TODAVIA NO FUNCIONA BIEN.

ntraj=0;
%==========================================================================
% SETEAMOS EN LA ESTRUCTURA DE LOS MINIMOS 2 VALORES QUE NECESITAMOS PARA
% EL CALCULO DE LA TRAYECTORIA.
%==========================================================================

for itime=1:size(MinStruct,2)
    for imin=1:MinStruct(itime).nminimos
    
        MinStruct(itime).asociado(imin)=false;  %Si el minimo esta asociado a alguna trayectoria.
        MinStruct(itime).trajectory(imin)=NaN;  %A que trayectoria esta asociado el minimo.
        MinStruct(itime).costfunction(imin)=NaN;%Con que cost function se asocio el minimo imin a la trayectoria trajectory.
        MinStruct(itime).trajectoryend(imin)=false; %Si la trayectoria termina en este minimo.
        
    end
end



%==========================================================================
% START TIME LOOP FOR TRAJECTORY COMPUTATION
%==========================================================================

for itime=2:size(MinStruct,2)-1  %Arranca en el tiempo 2 porque el primero no tiene sentido analizarlo.
    
 

    for imin=1:MinStruct(itime).nminimos
    % VAMOS A HACER EL ANALISIS DE CONTINUIDAD CENTRANDONOS EN CADA UNO DE
    % LOS MINIMOS.
    
       %OBTENGO LA LISTA DE LOS POSIBLES MINIMOS ASOCIABLES EN EL TIEMPO
       %T-1
       minprelist=[];
       for iminpre=1:MinStruct(itime-1).nminimos
          if( abs(MinStruct(itime-1).minlat(iminpre) - MinStruct(itime).minlat(imin)) < 10) %Condicion de descarte facil para acelerar la busqueda. 
            %Si esto sucede calculo la distancia exacta entre ambos.
           
           tmp_dist=distll_fun(MinStruct(itime-1).minlon(iminpre),MinStruct(itime-1).minlat(iminpre),MinStruct(itime).minlon(imin),MinStruct(itime).minlat(imin));
           
           if(tmp_dist < MaxTravelDistance)
               minprelist=[minprelist iminpre]; %#ok<AGROW>
           
          end
          end
       end

       %OBTENGO LA LISTA DE LOS POSIBLES MINIMOS ASOCIABLES EN EL TIEMPO
       %T+1
       minfowlist=[];
       
       for iminfow=1:MinStruct(itime+1).nminimos
          if( abs(MinStruct(itime+1).minlat(iminfow) - MinStruct(itime).minlat(imin)) < 10) %Condicion de descarte facil para acelerar la busqueda. 
            %Si esto sucede calculo la distancia exacta entre ambos.
           
           tmp_dist=distll_fun(MinStruct(itime+1).minlon(iminfow),MinStruct(itime+1).minlat(iminfow),MinStruct(itime).minlon(imin),MinStruct(itime).minlat(imin));
           
           if(tmp_dist < MaxTravelDistance)
               minfowlist=[minfowlist iminfow]; %#ok<AGROW>
           
          end
          end
       end

       %EN ESTE PUNT CONTAMOS CON 2 LISTAS DE MINIMOS POSIBLES EN EL TIEMPO
       %PASADO Y EN EL PRESENTE, AHORA VAMOS A EVALUAR TODAS LAS POSIBLES
       %COMBINACIONES QUE INVOLUCREN A ESTOS MINIMOS. 
       
       if( ~isempty(minfowlist) && ~isempty(minprelist) ) 
       %SI NO HAY MINIMOS POSIBLES EN EL TIEMPO PASADO O EN EL TIEMPO
       %FUTURO NO HAY NADA QUE HACER. 
       
          %DEFINO LAS POSIBLES COMBINACIONES DE MINIMOS PASADOS Y FUTUROS.
          comb=NaN(length(minprelist)*length(minfowlist),3);
          ncomb=0;
          for ii=1:length(minprelist)
              for jj=1:length(minfowlist)
                  ncomb=ncomb+1;
                  comb(ncomb,:)=[minprelist(ii) imin minfowlist(jj)];
              end
          end
           
           
          %CALCULO LA COST FUNCTION PARA CADA UNA DE ESTAS COMBINACIONES...
          %LA COSTFUNCTION SE BASA EN UN CRITERIO DE CONTINUIDAD
          %DIRECCIONAL Y DE VELOCIDADES.
          costfunction=NaN(1,size(comb,1));
          for ii=1:size(comb,1);
              lona=MinStruct(itime-1).minlon(comb(ii,1));
              lata=MinStruct(itime-1).minlat(comb(ii,1));
              lonb=MinStruct(itime).minlon(comb(ii,2));
              latb=MinStruct(itime).minlat(comb(ii,2));
              lonc=MinStruct(itime+1).minlon(comb(ii,3));
              latc=MinStruct(itime+1).minlat(comb(ii,3));
              
              deltat1=1; %No importa el delta t, lo importante es que es igual para ambos tiempos.
              deltat2=1;
              costfunction(ii)=cost_function_trajectory(lona,lata,lonb,latb,lonc,latc,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
          end
          
          %ME QUEDO SOLO CON LAS POSIBLES COMBINACIONES QUE TIENEN UN COST
          %FUNCTION MENOR QUE EL PERMITIDO.
          
          tmplog=costfunction < MaxCostFunction;
          costfunction=costfunction(tmplog);
          comb=comb(tmplog,:);
          
          %ORDENO LOS COST FUNCTION DE MENOR A MAYOR (ES DECIR DE LA MEJOR
          %CHANCE A LA PEOR CHANCE SOLO CONSIDERANDO AQUELLOS QUE ESTAN POR
          %DEBAJO DEL UMBRAL.
          
          [costfunction indexsort]=sort(costfunction,'ascend');
          comb=comb(indexsort,:);
          
          
          %AHORA DENTRO DE LAS POSIBLES TRAMOS DE TRAYECTORIAS VAMOS A
          %EVALUAR LAS DIFERENTES POSIBILIDADES, LA IDEA ES IR PROBANDO
          %PRIMERO CON LA QUE TIENE EL COST FUNCTION MAS BAJO, PERO SI ESA
          %NO LOGRA CONVERTIRSE EN LA CONTINUACION DE UNA TRAYECTORIA O EN
          %UNA NUEVA TRAYECTORIA POR X MOTIVO ENTONCES VAMOS A SEGUIR
          %PROBANDO CON LOS RESTANTES POSIBILIDADES. 
          
          for jj=1:length(costfunction) 
              % 1----------------------------------------------------------
              %CASO 1, NINGUNO DE LOS MINIMOS ESTA ASOCIADO A UNA
              %TRAYECTORIA... ENTONCES CREO UNA NUEVA TRAYECTORIA A PARTIR
              %DE ESTE GRUPO DE MINIMOS.
             
              
              iminpre=comb(jj,1);
              iminfow =comb(jj,3);
              imincur =comb(jj,2);
       
              
              if( ~MinStruct(itime-1).asociado(iminpre) && ~MinStruct(itime).asociado(imincur) && ~MinStruct(itime+1).asociado(iminfow) )
                  %POR AHORA SOLO GUARDAMOS LOS ID DE LOS MINIMOS.
                  ntraj=ntraj+1;
                  trajectory(ntraj).time(1:3)=[itime-1 itime itime+1];
                  trajectory(ntraj).minid(1:3)=[iminpre imincur iminfow];
                  
                  MinStruct(itime-1).asociado(iminpre)=true;
                  MinStruct(itime-1).trajectory(iminpre)=ntraj;
                  MinStruct(itime-1).trajectoryend(iminpre)=false;
                  
                  MinStruct(itime).asociado(imincur)=true;
                  MinStruct(itime).trajectory(imincur)=ntraj;
                  MinStruct(itime).trajectoryend(imincur)=false;
                  MinStruct(itime).costfunction(imincur)=costfunction(jj);
                  
                  MinStruct(itime+1).asociado(iminfow)=true;
                  MinStruct(itime+1).trajectory(iminfow)=ntraj;
                  MinStruct(itime+1).trajectoryend(iminfow)=true;
                  
                  break  %Salgo del loop porque ya pude asociar el minimo de la mejor manera posible.
              end  %END SOBRE LA POSIBILIDAD 1.
             
              % 2----------------------------------------------------------
              % CASO 2, EL MINIMO T-1 Y T YA ESTABAN ASOCIADOS Y ADEMAS
              % ESTABAN ASOCIADOS A LA MISMA TRAYECTORIA y T+1 NO ESTABA
              % ASOCIADO... EN ESTE CASO CHEQUEO SI T+1 ESTA LIBRE, SI ESTA
              % ES LA CONTINUACION DE T-1, T, SINO TENGO QUE VER ...
              
              if( MinStruct(itime-1).asociado(iminpre) && MinStruct(itime).asociado(imincur) && MinStruct(itime-1).trajectory(iminpre)==MinStruct(itime).trajectory(imincur) )
                 extracond=false;
                 if(~MinStruct(itime+1).asociado(iminfow))
                 %ESTE ES EL CASO MAS DESEABLE, T+1 ESTA LIBRE ENTONCES ES
                 %LA CONTINUACION NATURAL DE T-1, T.
                 extracond=true;
                 else
                 %EN ESTE CASO T+1 YA FUE ASOCIADO A OTRA TRAYECTORIA.
                 %UBICO CUAL FUE Y COMPARA LOS COST FUNCTIONS PARA VER
                 %QUIEN SE QUEDA CON T+1.
                   
                   trajanterior=MinStruct(itime+1).trajectory(iminfow);
                   minimoanterior=trajectory(trajanterior).minid(end-1);
                   costfunctionanterior=MinStruct(itime).costfunction(minimoanterior);
                   
                   if( costfunctionanterior > costfunction(jj) )
                   %ESTO QUIERE DECIR QUE LA ASOCIACION QUE ESTAMOS
                   %PROPONIENDO ES MEJOR QUE LA QUE T+1 TENIA.
                   extracond=true;
                   %MODIFICO LA TRAJECTORIA ANTERIOR. (ELIMINO SU ULTIMO
                   %ELEMENTO)
                   trajectory(trajanterior).time(end)=[];
                   trajectory(trajanterior).minid(end)=[];
                   
                   MinStruct(itime).trajectoryend(minimoanterior)=true; 
                    
                   end
                   
                 end
 
                 if(extracond)
                   %NOW MODIFY CURRENT TRAJECTORY AND REDEFINE THE
                   %ASSOCIATION OF T+1;
                   curtraj=MinStruct(itime).trajectory(imincur);
                   
                   slot=length(trajectory(curtraj).time)+1;
                   trajectory(curtraj).time(slot)=itime+1;
                   trajectory(curtraj).minid(slot)=iminfow;
                   
                   MinStruct(itime+1).asociado(iminfow)=true;
                   MinStruct(itime+1).trajectory(iminfow)=MinStruct(itime).trajectory(imincur);
                   MinStruct(itime+1).trajectoryend(iminfow)=true;
                   MinStruct(itime).trajectoryend(imincur)=false;
                   MinStruct(itime).costfunction(imincur)=costfunction(jj);
                   
                   break  %Este minimo ya fue asociado de la mejor manera posible.

                 end
                 
              end  %END SOBRE LA POSIBILIDAD 2.
                 
              % 3 ---------------------------------------------------------
              % CASO 3, EL MINIMO T NO PERTENECE A NINGUNA TRAJECTORIA,
              % PERO T-1 SI Y ES EL FINAL DE ESA TRAJECTORIA. T+1 PUEDE O
              % NO PERTENECER A UNA TRAJECTORIA. SI PERTENECE SE VERA CUAL
              % ES MEJOR.
              
          if(MinStruct(itime-1).asociado(iminpre) && ~MinStruct(itime).asociado(imincur)  && ...
                 MinStruct(itime-1).trajectoryend(iminpre) )
              
              %LO PRIMERO QUE TENGO QUE CHEQUEAR ES EL COST FUNCTION DE
              %AGREGAR EL PUNTO T A LA TRAYECTORIA QUE PASA POR T-1.
              trajanterior=MinStruct(itime-1).trajectory(iminpre);
              %TENGO QUE OBTENER LAS LATS Y LONS DE LOS PUNTOS END Y END-1
              %DE ESTA TRAYECTORIA PARA PODER CALCULAR LA COST FUNCTION.
              tmp=trajectory(trajanterior).minid(end-1);
              
              lona=MinStruct(itime-2).minlat(tmp);
              lata=MinStruct(itime-2).minlon(tmp);
              tmp=trajectory(trajanterior).minid(end);
              lonb=MinStruct(itime-1).minlat(tmp);
              latb=MinStruct(itime-1).minlon(tmp);
              
              lonc=MinStruct(itime).minlon(imincur);
              latc=MinStruct(itime).minlat(imincur);
                 
              deltat1=1; %No importa el delta t, lo importante es que es igual para ambos tiempos.
              deltat2=1;
              tmpcostfunction=cost_function_trajectory(lona,lata,lonb,latb,lonc,latc,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
              
              if(tmpcostfunction < MaxCostFunction)
              %ESTA ES LA PRIMERA CONDICION QUE SE TIENE QUE CUMPLIR PARA QUE T Y T+1
              %PUEDAN SER LA CONTINUACION DE T-1 Y ES QUE AGREGAR T A LA
              %TRAJECTORIA QUE PASA POR T-1 SEA FACTIBLE EN TERMINOS DE LA
              %FUNCION DE COSTO.
              
              %AHORA RESTA VER SI T+1 ESTA LIBRE O BIEN SI ESTA ASOCIACION
              %ES MEJOR QUE LA QUE TIENE ACTUALMENTE.
              extracond=false;
                 if(~MinStruct(itime+1).asociado(iminfow))
                 %ESTE ES EL CASO MAS DESEABLE, T+1 ESTA LIBRE ENTONCES ES
                 %LA CONTINUACION NATURAL DE T-1, T.
                 extracond=true;
                 else
                 %EN ESTE CASO T+1 YA FUE ASOCIADO A OTRA TRAYECTORIA.
                 %UBICO CUAL FUE Y COMPARA LOS COST FUNCTIONS PARA VER
                 %QUIEN SE QUEDA CON T+1.
                   curtraj=MinStruct(itime-1).trajectory(iminpre);
                   trajanterior=MinStruct(itime+1).trajectory(iminfow);
                   minimoanterior=trajectory(trajanterior).minid(end-1);
                   costfunctionanterior=MinStruct(itime).costfunction(minimoanterior);
                   
                   if( costfunctionanterior > costfunction(jj) )
                   %ESTO QUIERE DECIR QUE LA ASOCIACION QUE ESTAMOS
                   %PROPONIENDO ES MEJOR QUE LA QUE T+1 TENIA.
                   extracond=true;
                   %MODIFICO LA TRAJECTORIA ANTERIOR. (ELIMINO SU ULTIMO
                   %ELEMENTO)
                   trajectory(trajanterior).time(end)=[];
                   trajectory(trajanterior).minid(end)=[];
                   
                   MinStruct(itime).trajectoryend(minimoanterior)=true; 
                    
                   end
                   
                 end
 
                 if(extracond)
                   %NOW MODIFY CURRENT TRAJECTORY AND REDEFINE THE
                   %ASSOCIATION OF T+1;
                   curtraj=MinStruct(itime-1).trajectory(iminpre);
                   
                   slot=length(trajectory(curtraj).time)+1;
                   trajectory(curtraj).time(slot)=itime;
                   trajectory(curtraj).minid(slot)=imincur;
                   slot=slot+1;
                   trajectory(curtraj).time(slot)=itime+1;
                   trajectory(curtraj).minid(slot)=iminfow;
                   
                   MinStruct(itime-1).trajectoryend(iminpre)=false;
                   MinStruct(itime).trajectoryend(imincur)=false;
                   MinStruct(itime).asociado(imincur)=true;
                   MinStruct(itime).trajectory(imincur)=curtraj;
                   MinStruct(itime+1).asociado(iminfow)=true;
                   MinStruct(itime+1).trajectory(iminfow)=curtraj;
                   MinStruct(itime+1).trajectoryend(iminfow)=true;
                   
                   MinStruct(itime).costfunction(imincur)=costfunction(jj);
                   
                   break  %Este minimo ya fue asociado de la mejor manera posible.

                 end
                  
              end
          end %END DEL DO SOBRE LA POSIBILIDAD 3. 
          
          end %END DEL DO SOBRE LOS VALORES DE LA COST FUNCTION PARA BUSCAR LA MEJOR POSIBILIDAD.

       end  %End sobre el if de si estan vacias las listas de minimos pasados y futuros.
       
    end %End del loop sobre los minimos del tiempo itime

end %End del loop sobre los tiempos.


%==========================================================================
% CHEQUEO SI ALGUNAS TRAYECTORIAS QUE SE PARTIERON DEBIDO A UNA DISCONTINUIDAD, PODRIAN
% JUNTARSE SI UNO DE LOS PUNTOS EXTREMOS SE REEMPLAZAN POR UN PUNTO
% FICTICIO. ESTO SE HACE PARA EVITAR QUE CAMBIOS RUIDOSOS Y SUBITOS EN LA
% POSICION DEL MINIMO PUEDAN ROMPER PREMATURAMENTE UNA TRAYECTORIA.
%==========================================================================
if(prematurebreak)
fprintf('Chequeo si puedo pegar dos trayecotrias eliminando uno de sus elementos\n');
for it=1:ntraj
   for jt=it:ntraj 
   %VEO COMO ES EL TIEMPO DE FIN DE IT RESPECTO DEL INICIO DE JT.
   if(~isempty(trajectory(it).time) && ~isempty(trajectory(jt).time))
    if(trajectory(it).time(end) == trajectory(jt).time(1) - 1)
        %SI IT TERMINA UN TIEMPO ANTES QUE JT... ENTONCES
        %COMPARO LAS LATITUDES
              tiempo=trajectory(it).time(end);
              tmpa=trajectory(it).minid(end);
              tmpb=trajectory(jt).minid(1);
              lata=MinStruct(tiempo).minlat(tmpa);
              latb=MinStruct(tiempo+1).minlat(tmpb);
              lona=MinStruct(tiempo).minlon(tmpa);
              lonb=MinStruct(tiempo+1).minlon(tmpb);
              if(abs(lata-latb) < 10)  %SI ESTAN ASI DE CERCA CALCULO LA DISTANCIA TOTAL.
              %TENEMOS UNA POSIBLE CONTINUACION.
              tmpdist=distll_fun(lona,lata,lonb,latb);
               if(tmpdist < MaxTravelDistance)

                 %AHORA SI ESTAN DADAS TODAS LAS CONDICIONES PREVIAS, VAMOS
                 %A CHEQUEAR SI SACANDO EL ULTIMO PUNTO DE IT O EL PRIMERO
                 %DE JT Y REEMPLAZANDO POR UN PUNTO FICTICIO (INTERPOLADO)
                 %PODEMOS TENER UNA TRAYECTORIA RAZONABLEMENTE SUAVE.
                 % ----IT (END-2) ---- IT(END-1) ----IT (END)   JT(1)---JT(2)----JT(3)
                 %        IT1             IT2          IT3       JT1     JT2      JT3
                 %VAMOS A UTILIZAR LAT Y LON DE TODOS ESTOS PUNTOS EN EL
                 %CALCULO.
                 if(length(trajectory(it).time) > 3 && length(trajectory(jt).time) >3)
                 %fprintf('La trayectoria %10f es una posible continuacion de la trayectoria %10f \n',it,jt)
                   tiempo=trajectory(it).time(end);
                   tmp=trajectory(it).minid(end-2);
                   latit1=MinStruct(tiempo-2).minlat(tmp);
                   lonit1=MinStruct(tiempo-2).minlon(tmp);

                   tmp=trajectory(it).minid(end-1);
                   latit2=MinStruct(tiempo-1).minlat(tmp);
                   lonit2=MinStruct(tiempo-1).minlon(tmp);
                   minanomit2=MinStruct(tiempo-1).minanom(tmp);
                   minlapit2=MinStruct(tiempo-1).minlap(tmp);
                   minareait2=MinStruct(tiempo-1).minarea(tmp);
                   meananomit2=MinStruct(tiempo-1).meananom(tmp);
                   tmp=trajectory(it).minid(end);
                   latit3=MinStruct(tiempo).minlat(tmp);
                   lonit3=MinStruct(tiempo).minlon(tmp);
                   minanomit3=MinStruct(tiempo).minanom(tmp);
                   minlapit3=MinStruct(tiempo).minlap(tmp);
                   minareait3=MinStruct(tiempo).minarea(tmp);
                   meananomit3=MinStruct(tiempo).meananom(tmp);

                   tmp=trajectory(jt).minid(1);
                   latjt1=MinStruct(tiempo+1).minlat(tmp);
                   lonjt1=MinStruct(tiempo+1).minlon(tmp);
                   minareajt1=MinStruct(tiempo+1).minarea(tmp);
                   meananomjt1=MinStruct(tiempo+1).meananom(tmp);
                   minanomjt1=MinStruct(tiempo+1).minanom(tmp);
                   minlapjt1=MinStruct(tiempo+1).minlap(tmp);
                   tmp=trajectory(jt).minid(2);
                   latjt2=MinStruct(tiempo+2).minlat(tmp);
                   lonjt2=MinStruct(tiempo+2).minlon(tmp);
                   minareajt2=MinStruct(tiempo+2).minarea(tmp);
                   meananomjt2=MinStruct(tiempo+2).meananom(tmp);
                   minanomjt2=MinStruct(tiempo+2).minanom(tmp);
                   minlapjt2=MinStruct(tiempo+2).minlap(tmp);
                   tmp=trajectory(jt).minid(3);
                   latjt3=MinStruct(tiempo+3).minlat(tmp);
                   lonjt3=MinStruct(tiempo+3).minlon(tmp);
                   
                   %PRIMERO VAMOS A ASUMIR QUE SACAMOS EL ULTIMO PUNTO DE
                   %LA TRAYECTORIA IT Y VAMOS A CALCULAR LAS FUNCIONES DE
                   %COSTO J(IT1,IT2,IT3F),J(IT2,IT3F,IJ1),J(IT3N,IJ1,IJ2) y
                   %SU SUMA.
                   
                   %Creamos el punto ficticio IT3F
                   latit3f=0.5*(latit2+latjt1);
                   lonit3f=mean_lon_fun([lonit2 lonjt1]);
                   minanomit3f=0.5*(minanomit2+minanomjt1);
                   minlapit3f=0.5*(minlapit2+minlapjt1);
                   minareait3f=0.5*(minareait2+minareajt1);
                   meananomit3f=0.5*(meananomit2+meananomjt1);
                   
                   deltat1=1;
                   deltat2=1;
                   JI1=cost_function_trajectory(lonit1,latit1,lonit2,latit2,lonit3f,latit3f,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   %JI2=cost_function_trajectory(lonit2,latit2,lonit3f,latit3f,lonjt1,latjt1,deltat1,deltat2,MaxTravelDistance,CostFunctionType) 
                   JI3=cost_function_trajectory(lonit3f,latit3f,lonjt1,latjt1,lonjt2,latjt2,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   JIT=JI1+JI3;
                   
                   %REPITO LO MISMO PARA EL PUNTO FICTICIO IJF1
                   latjt1f=0.5*(latjt2+latit3);
                   lonjt1f=mean_lon_fun([lonjt2 lonit3]);
                   minanomjt1f=0.5*(minanomjt2+minanomit3);
                   minlapjt1f=0.5*(minlapjt2+minlapit3);
                   minareajt1f=0.5*(minareajt2+minareait3);
                   meananomjt1f=0.5*(meananomjt2+meananomit3);
                   deltat1=1;
                   deltat2=1;
                   JJ1=cost_function_trajectory(lonit2,latit2,lonit3,latit3,lonjt1f,latjt1f,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   %JJ2=cost_function_trajectory(lonit3,latit3,lonjt1f,latjt1f,lonjt2,latjt2,deltat1,deltat2,MaxTravelDistance,CostFunctionType)  
                   JJ3=cost_function_trajectory(lonjt1f,latjt1f,lonjt2,latjt2,lonjt3,latjt3,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   JJT=JJ1+JJ3;
                   
                   %AHORA TENGO QUE VER QUE OPCION ES LA MEJOR.
                   continuai=false;
                   continuaj=false;
                   if( JI1 < MaxCostFunction && JI3 < MaxCostFunction)
                       continuai=true;
                   end
                   if( JJ1 < MaxCostFunction && JJ3 < MaxCostFunction)
                       continuaj=true;
                   end
                   if( continuaj && continuai)
                       %Las dos posibilidades son permitidas por las cost
                       %function. Vemos cual suma menos de las 2.
                       if( JIT < JJT)
                           continuaj=false;
                       else
                           continuai=false;
                       end
                   end
                   %Si alguna de las 2 es true puedo fusionar las
                   %trayectorias.
                   if(continuai)  %CASO EN EL QUE USAMOS EL PUNTO FICTICIO IT3F.
                   fprintf('Junte la trayectoria %10f y la %10f \n',it,jt);
                     %Sacamos el ultimo elemento de la trayectoria i.
                     tiempo=trajectory(it).time(end);
                     %Creamos el minimo f3 en el array MinStruct.
                     tmp=MinStruct(tiempo).nminimos+1;
                     MinStruct(tiempo).nminimos=tmp;
                     MinStruct(tiempo).minlat(tmp)=latit3f;
                     MinStruct(tiempo).minlon(tmp)=lonit3f;
                     MinStruct(tiempo).minanom(tmp)=minanomit3f;
                     MinStruct(tiempo).minlap(tmp)=minlapit3f;
                     MinStruct(tiempo).asociado(tmp)=true;
                     MinStruct(tiempo).trajectory(tmp)=it;
                     MinStruct(tiempo).trajectoryend(tmp)=false;
                     MinStruct(tiempo).linked(tmp)=false;
                     MinStruct(tiempo).linkedmin(tmp)=NaN;
                     %Saco el ultimo minimo de la trayectoria it.
                     tmpmin=trajectory(it).minid(end);
                     MinStruct(tiempo).asociado(tmpmin)=false;
                     MinStruct(tiempo).trajectory(tmpmin)=NaN;
                     MinStruct(tiempo).trajectoryend(tmpmin)=false;
                     MinStruct(tiempo).minarea(tmp)=minareait3f;
                     MinStruct(tiempo).meananom(tmp)=meananomit3f;
                     
                     %Agrego el punto ficticio a la trayectoria it.
                     trajectory(it).minid(end)=tmp; %#ok<AGROW>
                     %Cambio la asociacion de todos los minimos de la
                     %trayectoria jt.
                       for kk=1:length(trajectory(jt).time)
                          tmpmin =trajectory(jt).minid(kk);
                          tmptime=trajectory(jt).time(kk);
                          MinStruct(tmptime).trajectory(tmpmin)=it;
                       end
                     trajectory(it).minid=[trajectory(it).minid trajectory(jt).minid];%#ok<AGROW>
                     trajectory(it).time= [trajectory(it).time  trajectory(jt).time];%#ok<AGROW>
                     %Elimino la trayectoria it
                     trajectory(jt).minid=[];%#ok<AGROW>
                     trajectory(jt).time =[];%#ok<AGROW>
                     
                   end
                   
                  if(continuaj)  %CASO EN EL QUE USAMOS EL PUNTO FICTICIO JT1F
                  fprintf('Junte la trayectoria %10f y la %10f \n',it,jt);
                     %Sacamos el primer elemento de la trayectoria jt.
                     tiempo=trajectory(jt).time(1);
                     %Creamos el minimo f1 en el array MinStruct.
                     tmp=MinStruct(tiempo).nminimos+1;
                     MinStruct(tiempo).nminimos=tmp;
                     MinStruct(tiempo).minlat(tmp)=latjt1f;
                     MinStruct(tiempo).minlon(tmp)=lonjt1f;
                     MinStruct(tiempo).minanom(tmp)=minanomjt1f;
                     MinStruct(tiempo).minlap(tmp)=minlapjt1f;
                     MinStruct(tiempo).asociado(tmp)=true;
                     MinStruct(tiempo).trajectory(tmp)=jt;
                     MinStruct(tiempo).trajectoryend(tmp)=false;
                     MinStruct(tiempo).linked(tmp)=false;
                     MinStruct(tiempo).linkedmin(tmp)=NaN;
                     %Saco el ultimo minimo de la trayectoria it.
                     tmpmin=trajectory(jt).minid(1);
                     MinStruct(tiempo).asociado(tmpmin)=false;
                     MinStruct(tiempo).trajectory(tmpmin)=NaN;
                     MinStruct(tiempo).trajectoryend(tmpmin)=false;
                     MinStruct(tiempo).minarea(tmp)=minareajt1f;
                     MinStruct(tiempo).meananom(tmp)=meananomjt1f;
                     
                     %Agrego el punto ficticio a la trayectoria jt.
                     trajectory(jt).minid(1)=tmp; %#ok<AGROW>
                     %Cambio la asociacion de todos los minimos de la
                     %trayectoria it.
                       for kk=1:length(trajectory(it).time)
                          tmpmin =trajectory(it).minid(kk);
                          tmptime=trajectory(it).time(kk);
                          MinStruct(tmptime).trajectory(tmpmin)=jt;
                       end
                     %Fusiono las 2 trayectorias en la trayectoria jt.
                     trajectory(jt).minid=[trajectory(it).minid trajectory(jt).minid]; %#ok<AGROW>
                     trajectory(jt).time= [trajectory(it).time  trajectory(jt).time]; %#ok<AGROW>
                     %Elimino la trayectoria it
                     trajectory(it).minid=[]; %#ok<AGROW>
                     trajectory(it).time =[]; %#ok<AGROW>
                     
                   end
                   
                   
                 end
               end
              end
    end
   end
   end
end


end

%==========================================================================
% CHEQUEO SI ALGUNAS TRAYECTORIAS QUE SE SUPERPONEN EN UN TIEMPO PODRIAN
% SER REDEVELOPMENTS DE UN MISMO SISTEMA Y PODRIAN ASOCIARSE A UNA SOLA
% TRAYECTORIA.
%==========================================================================
if(redevelopment)
fprintf('Chequeo si puedo pegar dos trayectorias que se superponen en un punto\n');
for it=1:ntraj
   for jt=it:ntraj 
   %VEO COMO ES EL TIEMPO DE FIN DE IT RESPECTO DEL INICIO DE JT.
   if(~isempty(trajectory(it).time) && ~isempty(trajectory(jt).time))
    if(trajectory(it).time(end) == trajectory(jt).time(1))
        %SI IT TERMINA AL MISMO TIEMPO QUE JT INICIA... ENTONCES
        %COMPARO LAS LATITUDES
              tiempo=trajectory(it).time(end);
              tmpa=trajectory(it).minid(end);
              tmpb=trajectory(jt).minid(1);
              lata=MinStruct(tiempo).minlat(tmpa);
              latb=MinStruct(tiempo).minlat(tmpb);
              lona=MinStruct(tiempo).minlon(tmpa);
              lonb=MinStruct(tiempo).minlon(tmpb);
              if(abs(lata-latb) < 10)  %SI ESTAN ASI DE CERCA CALCULO LA DISTANCIA TOTAL.
              %TENEMOS UNA POSIBLE CONTINUACION.
              tmpdist=distll_fun(lona,lata,lonb,latb);
               if(tmpdist < MaxTravelDistance)

                 %AHORA SI ESTAN DADAS TODAS LAS CONDICIONES PREVIAS, VAMOS
                 %A CHEQUEAR SI REEMPLAZANDO EL ULTIMO PUNTO DE IT Y EL PRIMERO DE JT
                 %POR LA MEDIA DE AMBOS PUEDO TENER UNA TRAYECTORIA
                 %PODEMOS TENER UNA TRAYECTORIA RAZONABLEMENTE SUAVE.
                 % ----IT (END-2) ---- IT(END-1) ----IT (END)   JT(1=END)---JT(2)----JT(3)
                 %        IT1             IT2          IT3       JT1     JT2      JT3
                 %VAMOS A UTILIZAR LAT Y LON DE TODOS ESTOS PUNTOS EN EL
                 %CALCULO.
                 if(length(trajectory(it).time) > 3 && length(trajectory(jt).time) >3)
                 %fprintf('La trayectoria %10f es una posible continuacion de la trayectoria %10f \n',it,jt)
                   tiempo=trajectory(it).time(end);
                   tmp=trajectory(it).minid(end-2);
                   latit1=MinStruct(tiempo-2).minlat(tmp);
                   lonit1=MinStruct(tiempo-2).minlon(tmp);

                   tmp=trajectory(it).minid(end-1);
                   latit2=MinStruct(tiempo-1).minlat(tmp);
                   lonit2=MinStruct(tiempo-1).minlon(tmp);
                   tmp=trajectory(it).minid(end);
                   latit3=MinStruct(tiempo).minlat(tmp);
                   lonit3=MinStruct(tiempo).minlon(tmp);
                   minanomit3=MinStruct(tiempo).minanom(tmp);
                   minlapit3=MinStruct(tiempo).minlap(tmp);
                   minareait3=MinStruct(tiempo).minarea(tmp);
                   meananomit3=MinStruct(tiempo).meananom(tmp);

                   tmp=trajectory(jt).minid(1);
                   latjt1=MinStruct(tiempo).minlat(tmp);
                   lonjt1=MinStruct(tiempo).minlon(tmp);
                   minareajt1=MinStruct(tiempo).minarea(tmp);
                   meananomjt1=MinStruct(tiempo).meananom(tmp);
                   minanomjt1=MinStruct(tiempo).minanom(tmp);
                   minlapjt1=MinStruct(tiempo).minlap(tmp);
                   tmp=trajectory(jt).minid(2);
                   latjt2=MinStruct(tiempo+1).minlat(tmp);
                   lonjt2=MinStruct(tiempo+1).minlon(tmp);
                   tmp=trajectory(jt).minid(3);
                   latjt3=MinStruct(tiempo+2).minlat(tmp);
                   lonjt3=MinStruct(tiempo+2).minlon(tmp);
                   
                   
                   %Creamos el punto ficticio IT3F
                   latit3f=0.5*(latit3+latjt1);
                   lonit3f=mean_lon_fun([lonit3 lonjt1]);
                   minanomit3f=0.5*(minanomit3+minanomjt1);
                   minlapit3f=0.5*(minlapit3+minlapjt1);
                   minareait3f=0.5*(minareait3+minareajt1);
                   meananomit3f=0.5*(meananomit3+meananomjt1);
                   
                   deltat1=1;
                   deltat2=1;
                   JI1=cost_function_trajectory(lonit1,latit1,lonit2,latit2,lonit3f,latit3f,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   %JI2=cost_function_trajectory(lonit2,latit2,lonit3f,latit3f,lonjt1,latjt1,deltat1,deltat2,MaxTravelDistance,CostFunctionType) 
                   JI3=cost_function_trajectory(lonit3f,latit3f,lonjt2,latjt2,lonjt3,latjt3,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   JIT=JI1+JI3;

                   %AHORA TENGO QUE VER QUE OPCION ES LA MEJOR.
                   continuai=false;
                   if( JI1 < MaxCostFunction && JI3 < MaxCostFunction)
                       continuai=true;
                   end

                   %PUEDO FUSIONAR LAS DOS TRAYECTORIAS.
                   if(continuai)  %CASO EN EL QUE USAMOS EL PUNTO FICTICIO IT3F.
                   fprintf('Junte la trayectoria %10f y la %10f \n',it,jt);
                     %Sacamos el ultimo elemento de la trayectoria i.
                     tiempo=trajectory(it).time(end);
                     %Creamos el minimo f3 en el array MinStruct.
                     tmp=MinStruct(tiempo).nminimos+1;
                     MinStruct(tiempo).nminimos=tmp;
                     MinStruct(tiempo).minlat(tmp)=latit3f;
                     MinStruct(tiempo).minlon(tmp)=lonit3f;
                     MinStruct(tiempo).minanom(tmp)=minanomit3f;
                     MinStruct(tiempo).minlap(tmp)=minlapit3f;
                     MinStruct(tiempo).asociado(tmp)=true;
                     MinStruct(tiempo).trajectory(tmp)=it;
                     MinStruct(tiempo).trajectoryend(tmp)=false;
                     MinStruct(tiempo).linked(tmp)=false;
                     MinStruct(tiempo).linkedmin(tmp)=NaN;
                     MinStruct(tiempo).minarea(tmp)=minareait3f;
                     MinStruct(tiempo).meananom(tmp)=meananomit3f;
                     %Saco el ultimo minimo de la trayectoria it.
                     tmpmin=trajectory(it).minid(end);
                     MinStruct(tiempo).asociado(tmpmin)=false;
                     MinStruct(tiempo).trajectory(tmpmin)=NaN;
                     MinStruct(tiempo).trajectoryend(tmpmin)=false;
 
                     %Saco el primer minimo de la trayectoria jt.
                     tmpmin=trajectory(jt).minid(1);
                     MinStruct(tiempo).asociado(tmpmin)=false;
                     MinStruct(tiempo).trajectory(tmpmin)=NaN;
                     MinStruct(tiempo).trajectoryend(tmpmin)=false;
                    
                     %Agrego el punto ficticio a la trayectoria it.
                     trajectory(it).minid(end)=tmp; %#ok<AGROW>
                     %Cambio la asociacion de todos los minimos de la
                     %trayectoria jt.
                       for kk=2:length(trajectory(jt).time)
                          tmpmin =trajectory(jt).minid(kk);
                          tmptime=trajectory(jt).time(kk);
                          MinStruct(tmptime).trajectory(tmpmin)=it;
                       end
                     trajectory(it).minid=[trajectory(it).minid trajectory(jt).minid(2:end)];%#ok<AGROW>
                     trajectory(it).time= [trajectory(it).time  trajectory(jt).time(2:end)];%#ok<AGROW>
                     %Elimino la trayectoria jt
                     trajectory(jt).minid=[];%#ok<AGROW>
                     trajectory(jt).time =[];%#ok<AGROW>
                     
                   end
                 end
               end
              end
          end
       end
   end
end


end





%==========================================================================
% CHEQUEO SI DOS TRAYECTORIAS UNA QUE TERMINA EN UN TIEMPO T Y OTRA QUE
% COMIENZA EN UN TIEMPO T+2 PODRIA SER UNA LA CONTINUACION DE LA OTRA.
%==========================================================================
if(reconection)
fprintf('Chequeo si puede haber una continuacion en tiempos sucesivos\n');
for it=1:ntraj
   for jt=it:ntraj 
   %VEO COMO ES EL TIEMPO DE FIN DE IT RESPECTO DEL INICIO DE JT.
   if(~isempty(trajectory(it).time) && ~isempty(trajectory(jt).time))
    if(trajectory(it).time(end) == trajectory(jt).time(1) - 2)
        %SI IT TERMINA DOS TIEMPOS ANTES QUE JT... ENTONCES
        %COMPARO LAS LATITUDES
              tiempo=trajectory(it).time(end);
              tmpa=trajectory(it).minid(end);
              tmpb=trajectory(jt).minid(1);
              lata=MinStruct(tiempo).minlat(tmpa);
              latb=MinStruct(tiempo+2).minlat(tmpb);
              lona=MinStruct(tiempo).minlon(tmpa);
              lonb=MinStruct(tiempo+2).minlon(tmpb);
              if(abs(lata-latb) < 10)  %SI ESTAN ASI DE CERCA CALCULO LA DISTANCIA TOTAL.
              %TENEMOS UNA POSIBLE CONTINUACION.
              
              tmpdist=distll_fun(lona,lata,lonb,latb);
               if(tmpdist < 2*MaxTravelDistance)

                 %AHORA SI ESTAN DADAS TODAS LAS CONDICIONES PREVIAS, VAMOS
                 %A CHEQUEAR SI CREANDO UN PUNTO INTERMEDIO ENTRE AMBAS
                 %PODEMOS ASEGURAR QUE UNA ES LA CONTINUACION DE LA OTRA.
                 %  IT(END-1)---- IT(END) ----IF (ficticio)  JT(1)---JT(2)-
                 %     IT1          IT2          F             JT1     JT2
                 % 
                 %VAMOS A UTILIZAR LAT Y LON DE TODOS ESTOS PUNTOS EN EL
                 %CALCULO.
                 if(length(trajectory(it).time) > 3 && length(trajectory(jt).time) >3)
                 %fprintf('La trayectoria %10f es una posible continuacion de la trayectoria %10f \n',it,jt)
                   tiempo=trajectory(it).time(end);
                   tmp=trajectory(it).minid(end-1);
                   latit1=MinStruct(tiempo-1).minlat(tmp);
                   lonit1=MinStruct(tiempo-1).minlon(tmp);

                   tmp=trajectory(it).minid(end);
                   latit2=MinStruct(tiempo).minlat(tmp);
                   lonit2=MinStruct(tiempo).minlon(tmp);
                   %minanomit2=MinStruct(tiempo).minanom(tmp);
                   %minlapit2=MinStruct(tiempo).minlap(tmp);
                   meananomit2=MinStruct(tiempo).meananom(tmp);
                   minareait2=MinStruct(tiempo).minarea(tmp);
                   minanomit2=MinStruct(tiempo).minanom(tmp);
                   minlapit2=MinStruct(tiempo).minlap(tmp);
                   
                   
                   

                   tmp=trajectory(jt).minid(1);
                   latjt1=MinStruct(tiempo+2).minlat(tmp);
                   lonjt1=MinStruct(tiempo+2).minlon(tmp);
                   meananomjt1=MinStruct(tiempo+2).meananom(tmp);
                   minareajt1=MinStruct(tiempo+2).minarea(tmp);
                   minanomjt1=MinStruct(tiempo+2).minanom(tmp);
                   minlapjt1=MinStruct(tiempo+2).minlap(tmp);
                   tmp=trajectory(jt).minid(2);
                   latjt2=MinStruct(tiempo+3).minlat(tmp);
                   lonjt2=MinStruct(tiempo+3).minlon(tmp);

                   
                   %CREO EL PUNTO FICTICIO.
                   
                   %Creamos el punto ficticio IT3F
                   latf=0.5*(latit2+latjt1);
                   lonf=mean_lon_fun([lonit2 lonjt1]);
                   minanomf=0.5*(minanomit2+minanomjt1);
                   minlapf=0.5*(minlapit2+minlapjt1);
                   minareaf=0.5*(minareait2+minareajt1);
                   meananomf=0.5*(meananomit2+meananomjt1);
                   
                   
                   deltat1=1;
                   deltat2=1;
                   J1=cost_function_trajectory(lonit1,latit1,lonit2,latit2,lonf,latf,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   %JI2=cost_function_trajectory(lonit2,latit2,lonit3f,latit3f,lonjt1,latjt1,deltat1,deltat2,MaxTravelDistance,CostFunctionType) 
                   J3=cost_function_trajectory(lonf,latf,lonjt1,latjt1,lonjt2,latjt2,deltat1,deltat2,MaxTravelDistance,CostFunctionType);
                   
                   if( J1 < MaxCostFunction && J3 < MaxCostFunction)
                   %ESTO QUIERE DECIR QUE ESTAN DADAS LAS CONDICIONES PARA
                   %JUNTAR LAS TRAYECTORIAS.
                   fprintf('Junte la trayectoria %10f y la %10f \n',it,jt);
                     %Sacamos el ultimo elemento de la trayectoria i.
                     tiempo=trajectory(it).time(end)+1;
                     %Creamos el minimo f en el array MinStruct.
                     tmp=MinStruct(tiempo).nminimos+1;
                     MinStruct(tiempo).nminimos=tmp;
                     MinStruct(tiempo).minlat(tmp)=latf;
                     MinStruct(tiempo).minlon(tmp)=lonf;
                     MinStruct(tiempo).minanom(tmp)=minanomf;
                     MinStruct(tiempo).minlap(tmp)=minlapf;
                     MinStruct(tiempo).asociado(tmp)=true;
                     MinStruct(tiempo).trajectory(tmp)=it;
                     MinStruct(tiempo).trajectoryend(tmp)=false;
                     MinStruct(tiempo).linked(tmp)=false;
                     MinStruct(tiempo).linkedmin(tmp)=NaN;
                     MinStruct(tiempo).meananom(tmp)=meananomf;
                     MinStruct(tiempo).minarea(tmp)=minareaf;
                     
                     %Agrego el punto ficticio a la trayectoria it.
                     trajectory(it).minid(end+1)=tmp; %#ok<AGROW>
                     trajectory(it).time(end+1)=tiempo;
                    
                     %Cambio la asociacion de todos los minimos de la
                     %trayectoria jt.
                       for kk=1:length(trajectory(jt).time)
                          tmpmin =trajectory(jt).minid(kk);
                          tmptime=trajectory(jt).time(kk);
                          MinStruct(tmptime).trajectory(tmpmin)=it;
                       end
                     trajectory(it).minid=[trajectory(it).minid trajectory(jt).minid];%#ok<AGROW>
                     trajectory(it).time= [trajectory(it).time  trajectory(jt).time];%#ok<AGROW>
                     %Elimino la trayectoria it
                     trajectory(jt).minid=[];%#ok<AGROW>
                     trajectory(jt).time =[];%#ok<AGROW>
                     
                   end
  
                 end
               end
              end
    end
   end
   end
end

end
%==========================================================================
% TERMINO LA ASOCIACION DE LOS MINIMOS, AHORA RECONSTRUYO LAS TRAYECTORIAS
% EN FUNCION DE DICHA ASOCIACION
%==========================================================================

for it=1:ntraj %DO SOBRE LAS TRAJECTORIAS
    if(~isempty(trajectory(it).time))
    trajl=length(trajectory(it).time);
    for jj=1:trajl
        
        
        
        
      ctime=trajectory(it).time(jj);
      minid=trajectory(it).minid(jj);
      
      %CHECK
      if( MinStruct(ctime).trajectory(minid) ~= it | isnan(MinStruct(ctime).trajectory(minid)) )
      fprintf('Error, el minimo %10f, aparece asociado a la trayectoria %10f pero ese minimo en realidad esta asociado a la trayectoria %10f\n',minid,it,MinStruct(ctime).trajectory(minid))    
      end
      
      %trajectory(it).minid(jj)=trajectory(it).minid(jj);
      %trajectory(it).time(jj)=trajectory(it).time(jj);
      trajectory(it).daten(jj)=MinStruct(ctime).daten;
      trajectory(it).minlatf(jj)=MinStruct(ctime).minlat(minid);
      trajectory(it).minlonf(jj)=MinStruct(ctime).minlon(minid);
      trajectory(it).minlat(jj)=MinStruct(ctime).minlat(minid); %Repito estos campos para mantener la consistencia con versiones anteriores.
      trajectory(it).minlon(jj)=MinStruct(ctime).minlon(minid);
      trajectory(it).minlap(jj)=MinStruct(ctime).minlap(minid);
      trajectory(it).minanomf(jj)=MinStruct(ctime).minanom(minid);
      
      trajectory(it).meananomf(jj)=MinStruct(ctime).meananom(minid);
      trajectory(it).minareaf(jj)=MinStruct(ctime).minarea(minid);
      trajectory(it).minarea(jj)=MinStruct(ctime).minarea(minid);
      trajectory(it).meananom(jj)=MinStruct(ctime).meananom(minid);
      
      if(jj >=2)
      lona=trajectory(it).minlonf(jj-1);
      lata=trajectory(it).minlatf(jj-1);
      lonb=trajectory(it).minlonf(jj);
      latb=trajectory(it).minlatf(jj);
      deltat=(trajectory(it).daten(jj) - trajectory(it).daten(jj-1))*86400; %Delta T in seconds.;
      [tmpu tmpv tmpvel]=compute_uv_fun(lona,lata,lonb,latb,deltat);
      trajectory(it).uvelf(jj)=tmpu;
      trajectory(it).vvelf(jj)=tmpv;
      trajectory(it).velf(jj)=tmpvel;
      
      else
          
      trajectory(it).uvelf(jj)=NaN;
      trajectory(it).vvelf(jj)=NaN;
      trajectory(it).velf(jj)=NaN;   
          
          
      end

      trajectory(it).minanomsisf(jj)=NaN; %#ok<AGROW>
      trajectory(it).meananomsisf(jj)=NaN; %#ok<AGROW>

    end
    
    end
end            %END DEL DO SOBRE LAS TRAJECTORIAS


%==========================================================================
% ANALISIS DE LAS TRAJECTORIAS PARA DETECTAR MERGE, SPLIT Y OTRAS COSAS
% RARAS.
%==========================================================================

if(trajsurgery)

for it=1:ntraj %DO SOBRE LAS TRAJECTORIAS
    if(~isempty(trajectory(it).time))
    trajl=length(trajectory(it).time);
    
    isasoc=false(length(trajectory(it).time),1);
    asoctr=NaN(length(trajectory(it).time),1);
    for jj=1:trajl
            
      ctime=trajectory(it).time(jj);
      minid=trajectory(it).minid(jj);

      if(MinStruct(ctime).linked(minid))
          %Obtengo el numero de trayectoria asociada.
          lmin=MinStruct(ctime).linkedmin(minid);
          if(MinStruct(ctime).asociado(lmin))  %El minimo linkeado tiene una traj asociada?
            asoctr(jj)=MinStruct(ctime).trajectory(lmin);
            isasoc(jj)=true;
          end
      end
    end
    %HASTA ACA TENEMOS 2 VECOTORES ASOCTR Y ISASOC QUE PARA CADA TIEMPO DE
    %LA TRAJECTORIA IT NOS DICEN SI ESTA ASOCIADA CON OTRA TRAYECTORIA Y
    %CON CUAL.
    
    %VAMOS A HACER UN LOOP SOBRE TODAS LAS TRAJECTORIAS CON LAS QUE ESTA
    %ASOCIADA ANALIZANDO CADA CASO.
    
    if( any(isasoc) )  %SI HAY ALGUNA TRAYECTORIA ASOCIADA... 
    trasoc=unique(asoctr);  %Genero la lista con todas las trayectorias asociadas.
    trasoc=trasoc(~isnan(trasoc) & trasoc~=it );
        for iasoc=1:length(trasoc)
        %ANALIZO CADA CASO.
        if(~isempty(trajectory(jt).time))
        fprintf('La trayectoria %10f esta asociada a la trayectoria %10f\n',it,trasoc(iasoc));
        jt=trasoc(iasoc);
        %CASO 1: ANALIZO SI SE TRATA DE UN NUEVO DESARROLLO CON
        %SUPERPOSICION TEMPORAL DE AMBAS TRAYECTORIAS.
        suptime=trajectory(it).time(asoctr==jt);
        
        %Tengo que encontrar los subindices que corresponden a esos tiempos
        %para cada trayectoria.
        [int indexjt]=intersect(trajectory(jt).time,suptime);
        [int indexit]=intersect(trajectory(it).time,suptime);

        if(min(indexjt) <= 2 && max(indexit) >= length(trajectory(it).time) -2)
        fprintf('La trayectoria %10f es un redevelopment con superposicion de la trayectoria %10f\n',trasoc(iasoc),it);
        %ESTO QUIERE DECIR QUE LA SUPERPOSICION SE DA CERCA DEL PRINCIPIO
        %DE IT Y CERCA DEL FINAL DE JT.
           kkmin=1;
           kkmax=max(indexit)-min(indexit);
           for kk=kkmin:kkmax
              iti=min(indexit)+kk-1;
              jti=min(indexjt)+kk-1;
              trajectory(it).minid(kk)=-9;
              trajectory(it).minlatf(kk)=0.5*(trajectory(it).minlatf(iti)+trajectory(jt).minlatf(jti));
              trajectory(it).minlonf(kk)=mean_lon_fun([trajectory(it).minlonf(iti) trajectory(jt).minlonf(jti)]);
              trajectory(it).minlat(kk)=trajectory(it).minlatf(kk); 
              trajectory(it).minlon(kk)=trajectory(it).minlonf(kk);
              trajectory(it).minlap(kk)=0.5*(trajectory(it).minlap(iti)+trajectory(jt).minlap(jti));
              trajectory(it).minarea(kk)=0.5*(trajectory(it).minarea(iti)+trajectory(jt).minarea(jti));
              trajectory(it).meananom(kk)=0.5*(trajectory(it).meananom(iti)+trajectory(jt).meananom(jti));
              trajectory(it).minanomf(kk)=min([trajectory(it).minanomf(iti) trajectory(jt).minanomf(jti)]);
              trajectory(it).minareaf(kk)=trajectory(it).minarea;
           end
           %PEGO LO QUE QUEDA DE LA TRAYECTORIA JT A CONTINUACION DE LA
           %TRAJECTORIA IT.
           tmp=max(indexjt)+1;
           trajectory(it).time=[trajectory(it).time trajectory(jt).time(tmp:end)];
           trajectory(it).daten=[trajectory(it).daten trajectory(jt).daten(tmp:end)];
           trajectory(it).minid=[trajectory(it).minid trajectory(jt).minid(tmp:end)];
           trajectory(it).minlatf=[trajectory(it).minlatf trajectory(jt).minlatf(tmp:end)];
           trajectory(it).minlonf=[trajectory(it).minlonf trajectory(jt).minlonf(tmp:end)];
           trajectory(it).minlap=[trajectory(it).minlap trajectory(jt).minlap(tmp:end)];
           trajectory(it).minanomf=[trajectory(it).minanomf trajectory(jt).minanomf(tmp:end)];
           trajectory(it).minareaf=[trajectory(it).minareaf trajectory(jt).minareaf(tmp:end)];
           trajectory(it).meananomf=[trajectory(it).meananomf trajectory(jt).meananomf(tmp:end)];
           trajectory(it).uvelf=[trajectory(it).uvelf trajectory(jt).uvelf(tmp:end)];
           trajectory(it).vvelf=[trajectory(it).vvelf trajectory(jt).vvelf(tmp:end)];
           trajectory(it).velf=[trajectory(it).velf trajectory(jt).velf(tmp:end)];
           trajectory(it).minarea=[trajectory(it).minarea trajectory(jt).minarea(tmp:end)];
           trajectory(it).meananom=[trajectory(it).meananom trajectory(jt).meananom(tmp:end)];
           
           %ELIMINO LA TRAYECTORIA JT.
           for kk=1:length(trajectory(jt).time)
           tmpmin =trajectory(jt).minid(kk);
           tmptime=trajectory(jt).time(kk);
           if(tmpmin >0)
           MinStruct(tmptime).trajectory(tmpmin)=it;
           end
           end
           
           trajectory(jt).time=[];
           trajectory(jt).daten=[];
           trajectory(jt).minid=[];
           trajectory(jt).minlatf=[];
           trajectory(jt).minlonf=[];
           trajectory(jt).minlat=[];
           trajectory(jt).minlon=[];
           trajectory(jt).minlap=[];
           trajectory(jt).minanomf=[];
           trajectory(jt).minareaf=[]; 
           trajectory(jt).uvelf=[];
           trajectory(jt).vvelf=[];
           trajectory(jt).velf=[];
           trajectory(jt).minarea=[];
           trajectory(jt).meananom=[];
        end
    
            
        end
        end
    end
    end
end            %END DEL DO SOBRE LAS TRAJECTORIAS

end

%==========================================================================
% THIS PART OF THE CODE DOES SOME ADDITIONAL COMPUTATION AS FORECAST LEAD
% TIME (IN CASE OF FORECAST MODE), TRAJECTORY LENGTH, START DATE, END DATE
%==========================================================================

ntraj=size(trajectory,2);
   for itraj=1:ntraj
      %Compute length.
      if( ~isempty(trajectory(itraj).minlat))
      trajectory(itraj).length=length(trajectory(itraj).minlat);       %#ok<AGROW>
      trajectory(itraj).startdate=trajectory(itraj).daten(1);          %#ok<AGROW>
      trajectory(itraj).enddate=trajectory(itraj).daten(end);          %#ok<AGROW>
      %Forecast lead time in hours (in case of analysis, time since
      %trajectory computation initialization);
      trajectory(itraj).leadtime=(trajectory(itraj).daten-MinStruct(1).daten)*24;      %#ok<AGROW>
      end
   end


%VERIFICAR SI 2 TRAYECTORIAS QUE TIENEN UN BACHE DE UN TIEMPO ENTRE MEDIO
%PODRIAN SER UNA LA CONTINUACION DE LA OTRA.





