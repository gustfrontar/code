
function [trayectorias]=trayectory_fun(config)
%This function associates minimuns at different times to compute the
%trajectory of the different systems.

MaxActiveTrajectories=800;       %Maximum number of active trayectories that can be handled by this function.
                                 %Note that there is no limitation to the total
                                 %number of trajectories, this only limits
                                 %the number of trajectories that can be updated
                                 %at each time step.
SmoothTrajectory=true;           %If the trayectory track will be smoothd (results are stored into a new field).
SmoothSteps=1;                   %Number of times that the smoothing will be performed.
DetectIsolatedBadPoints=false;    %Detect sharp changes in trajectories associated with only on point
MaxWestwardPropagationSpeed=-10; %Maximun Westward propagation speed (u component only m/s)
MaxPropagationSpeed=33;          %Maximun propagation speed in m/s
MaxTravelDistance=MaxPropagationSpeed*config.timefrec*3600;   %Maximun distance that a system can travel between two frames.
AssociateShortTrajectories=false; %If short trajectories (i.e. 1 time long will be associated with longer ones).
IfContinuityCheck=true;           %Perform (or not) continuity check (recommended)
TrContinuity=15;                  %Threshold to decide wether to split or not a particular trayectory.
IfRedevelopmentSuperpossitionCheck=false;   %Check for redevelopment superposittion. (not a real problem for trajectory computation but for 
                                           %further trajectory association.
%==========================================================================
% GET MINIMUN STRUCT
%==========================================================================

[MinStruct]=get_minimun_struct(config);
         
%==========================================================================
% START TIME LOOP FOR TRAJECTORY COMPUTATION
%==========================================================================

for itime=1:size(MinStruct,2)
 

% IF WE ARE AT THE FIRST TIME WE ONLY INITIALIZE TRAYECTORIES.
%==========================================================================

 if(itime==1)

    for i=1:MinStruct(1).nminimos;
      trayectorias(i).minarea(1)=MinStruct(1).minarea(i); %#ok<AGROW>
      trayectorias(i).minlat(1)=MinStruct(1).minlat(i); %#ok<AGROW>
      trayectorias(i).minlon(1)=MinStruct(1).minlon(i); %#ok<AGROW>
      trayectorias(i).uvel(1)=NaN;   %#ok<AGROW> %We don't have enougth information for velocity computation yet. 
      trayectorias(i).vvel(1)=NaN;   %#ok<AGROW> %We don't have enougth information for velocity computation yet. 
      trayectorias(i).vel(1)=NaN; %#ok<AGROW>
      trayectorias(i).minanomsis(1)=MinStruct(1).minanomsis(i); %#ok<AGROW>
      trayectorias(i).meananomsis(1)=MinStruct(1).meananomsis(i); %#ok<AGROW>
      trayectorias(i).daten(1)=MinStruct(1).daten; %#ok<AGROW>
      if(MinStruct(1).additionaldataflag)
          trayectorias(i).minadditionaldata(1,:)=MinStruct(1).minadditionaldata(i,:); %#ok<AGROW>
          trayectorias(i).maxadditionaldata(1,:)=MinStruct(1).maxadditionaldata(i,:); %#ok<AGROW>
          trayectorias(i).meanadditionaldata(1,:)=MinStruct(1).meanadditionaldata(i,:); %#ok<AGROW>
      end
    end
    

 else  %Si no es el primer tiempo...
     
% ESTA PARTE DEL ALGORITMO ES EL MATCHING ENTRE LOS SISTEMAS DETECTADOS EN 
% EL TIEMPO T Y LOS DETECTADOS EN EL TIEMPO T-1. NOTAR QUE ESTA PARTE ES
% INDEPENDIENTE DE COMO DETECTAMOS LOS SISTEMAS.
% LA IDEA ES BARRER TODOS LOS SISTEMAS DETECTADOS EN EL TIEMPO T Y VER SI
% ESTAN CERCA DE ALGUNO DETECTADO EN EL TIEMPO T-1, SI UN SISTEMA DETECTADO
% EN EL TIEMPO T NO SE PUEDE ASOCIAR A NINGUN SISTEMA DETECTADO EN EL
% TIEMPO T-1 ENTONCES SE TRATA COMO UN NUEVO SISTEMA Y SE INICIALIZA UNA
% NUEVA TRAYECTORIA.
%========================================================================== 

    ntraj=length(trayectorias); %Get the number of trayectories so far.
    
    nmin=MinStruct(itime).nminimos; %Get the number of minimuns detected at the current time.
    
    %Initializae some variables:
    min_asoc=false(nmin,1);       %This will kep information about if a minimun has been associated with a trajectory.
    min_asoc_traj=zeros(nmin,1);   %This will tell to which trajectory has been associated (if any).
    min_asoc_dist=NaN(nmin,1);     %This will tell the distance between the current minimun possition and the last point of the associated trayectory.
    min_asoc_uvel=NaN(nmin,1);
    min_asoc_vvel=NaN(nmin,1);
    min_asoc_vel=NaN(nmin,1);
    
    %======================================================================
    % FIRST STEP: LOOP OVER ALL THE MINIMUNS AND ALL THE ACTIVE
    % TRAYECTORIES TO COMPUTE DISTANCES (IN SPACE AND TIME)
    %======================================================================
    
    %Note that the number of trajectories initially include all the
    %available trajectories (the algoritm could be included if the search
    %is only performed over the samble of active trayectories).
    
    %In this loop we will construct a matrix with the distance between
    %different minimuns and different trayectories. The rows will
    %correspond to different minimuns and the columns to different
    %trajectories.
    
    %Save a lot of space in memory for the matrix
    distance_matrix=NaN(nmin,MaxActiveTrajectories);  %Keep the information about the distance between each trajectory and the current minimuns.
    tdistance_matrix=NaN(nmin,MaxActiveTrajectories);
    uvel_matrix=NaN(nmin,MaxActiveTrajectories);
    vvel_matrix=NaN(nmin,MaxActiveTrajectories);
    vel_matrix=NaN(nmin,MaxActiveTrajectories);
    trajectory_index=NaN(MaxActiveTrajectories);      %Which trajectory corresponds to each column of the matrix
 
    for imin=1:nmin
        trajcounter=0;
        for itraj=1:ntraj
            %Check if the trajectory is active (i.e. if it has been
            %recently updated).
            if(MinStruct(itime).daten - trayectorias(itraj).daten(end) <= 2*config.timefrec/24);
               trajcounter=trajcounter+1;
                
               trajectory_index(trajcounter)=itraj;
               minlat=MinStruct(itime).minlat(imin);
               minlon=MinStruct(itime).minlon(imin);
               trajlat=trayectorias(itraj).minlat(end);
               trajlon=trayectorias(itraj).minlon(end);
               deltat=(MinStruct(itime).daten - trayectorias(itraj).daten(end))*86400;  %Delta T in seconds.;
               tdistance_matrix(imin,trajcounter)=24*(MinStruct(itime).daten - trayectorias(itraj).daten(end)); %Delta T in hours.
               distance_matrix(imin,trajcounter)=distll_fun(minlon,minlat,trajlon,trajlat);
               [uvel_matrix(imin,trajcounter) vvel_matrix(imin,trajcounter) vel_matrix(imin,trajcounter)]=compute_uv_fun(trajlon,trajlat,minlon,minlat,deltat);
            end 
        end  
    end

    %======================================================================
    % SECOND STEP: FROM ALL THE DATA THAT WE HAVE GATTERED ABOUT THE
    % MINIMUN AND THE ACTIVE TRAYECTORIES, EVALUATE WHICH MINIMUNS WILL BE
    % THE BEST CONTINUATION FOR EACH ACTIVE TRAYECTORY.
    %======================================================================
    
    %THE ORDER OF THE CONDITIONS ESTABLISH THE PRIORITY OF THE
    %ASSOCIATIONS. I.E. I WOULD PREFER TO ASSOCIATE SYSTEMS USING CRITERIA
    %A, BUT IF SOME OF THEM CANNOT BE ASSOCIATED IN THIS WAY WE TRY TO DO
    %THAT USING CRITERIA B AND SO ON.
    %A) CHECK FOR FOWARD RECENT (LAS TIME) AND CLOSE CONTINUATIONS.
    [MinDistRows MinDistLocRows]=min(distance_matrix,[],2);
    [MinDistCols MinDistLocCols]=min(distance_matrix,[],1); %#ok<ASGLU>

    for imin=1:nmin
       if(MinDistRows(imin) < MaxTravelDistance && MinDistLocCols(MinDistLocRows(imin))==imin ...
          && tdistance_matrix(imin,MinDistLocRows(imin))==config.timefrec ...
          && uvel_matrix(imin,MinDistLocRows(imin)) > 0)
          %This means that the distance between system imin and trajectory MinDistLocRows(imin)
          %is less than the desidered threshold. And also that this minimun
          %is the closest minimun to trajectory MinDistLocRows(imin) and
          %that trajectory MinDistLocRows(imin) is the closest trayectory
          %to minimun imin.
          %And the trayectory has also been active at the previous time and
          %the displacement of the system is to the west.
          
          %This is the perfect scenario!! 
          min_asoc(imin)=true;
          min_asoc_traj(imin)=trajectory_index(MinDistLocRows(imin));
          min_asoc_dist(imin)=MinDistRows(imin);
          min_asoc_uvel(imin)=uvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vvel(imin)=vvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vel(imin)=vel_matrix(imin,MinDistLocRows(imin));


          
       end    
    end
    %B) CHECK FOR FOWARD AND CLOSE CONTINUATIONS BUT NOT RECENT (UP TO TWO
    %TIMES OLD).
    for imin=1:nmin
       if(~min_asoc(imin) && MinDistRows(imin) < 1.5*MaxTravelDistance && MinDistLocCols(MinDistLocRows(imin))==imin ...
          && tdistance_matrix(imin,MinDistLocRows(imin))==2*config.timefrec ...
          && uvel_matrix(imin,MinDistLocRows(imin)) > 0)
          %This means that the distance between system imin and trajectory MinDistLocRows(imin)
          %is less than the desidered threshold. And also that this minimun
          %is the closest minimun to trajectory MinDistLocRows(imin) and
          %that trajectory MinDistLocRows(imin) is the closest trayectory
          %to minimun imin.
          %And the trayectory has also been active at the previous time and
          %the displacement of the system is to the west.
          
          %This association will be perform only if the trajectory is not
          %associated in any other way.
          if(sum(min_asoc_traj==trajectory_index(MinDistLocRows(imin))==0))
          min_asoc(imin)=true;
          min_asoc_traj(imin)=trajectory_index(MinDistLocRows(imin));
          min_asoc_dist(imin)=MinDistRows(imin);
          min_asoc_uvel(imin)=uvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vvel(imin)=vvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vel(imin)=vel_matrix(imin,MinDistLocRows(imin));
          end
       end    
    end
    %C) TRY TO FIND BACKWARD CONTINUATIONS THAT ARE CLOSE IN TIME.
    for imin=1:nmin
       if(~min_asoc(imin) && MinDistRows(imin) < MaxTravelDistance && MinDistLocCols(MinDistLocRows(imin))==imin ...
          && tdistance_matrix(imin,MinDistLocRows(imin))==config.timefrec ...
          && uvel_matrix(imin,MinDistLocRows(imin)) > MaxWestwardPropagationSpeed)
          %Same as condition A but the forward criteria is removed.
          
          %This association will be perform only if the trajectory is not
          %associated in any other way.
          if(sum(min_asoc_traj==trajectory_index(MinDistLocRows(imin))==0))
          min_asoc(imin)=true;
          min_asoc_traj(imin)=trajectory_index(MinDistLocRows(imin));
          min_asoc_dist(imin)=MinDistRows(imin);
          min_asoc_uvel(imin)=uvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vvel(imin)=vvel_matrix(imin,MinDistLocRows(imin));
          min_asoc_vel(imin)=vel_matrix(imin,MinDistLocRows(imin));
          end
       end    
    end
    %C) SEE IF THERE ARE MINIMUNS THAT CAN BE ASSOCIATED WITH FREE
    %TRAYECTORIES IN A FOWARD DIRECTION EVEN IF THE MATCHING THE BEST FROM
    %THE TRAYECTORY AND THE MINIMUN POINT OF VIEW AT THE SAME TIME.
    for imin=1:nmin
       mindistance=MaxTravelDistance; 
       if(~min_asoc(imin) && MinDistRows(imin) < mindistance)
         for itraj=1:size(distance_matrix,2)
            if(sum(min_asoc_traj==trajectory_index(itraj)==0)     && ...
                   distance_matrix(imin,itraj) < mindistance      &&...
                   tdistance_matrix(imin,itraj)==config.timefrec  &&...
                   uvel_matrix(imin,itraj) > MaxWestwardPropagationSpeed) 
                %THIS MEANS THAT THE MINIMUN HAS NOT BE ASSOCIATED, AND THE
                %TRAYECTORY ITRAJ IS AT TRAKING DISTANCE AND IT HASENT ALSO
                %BE ASSOCIATED.
                %THE WE ASSOCIATE THE MINIMUN IMIT WITH THE TRAYECTORY
                %TRAJECTORY_INDEX(ITRAJ)
                min_asoc(imin)=true;
                min_asoc_traj(imin)=trajectory_index(itraj);
                min_asoc_dist(imin)=tdistance_matrix(imin,itraj);
                min_asoc_uvel(imin)=uvel_matrix(imin,itraj);
                min_asoc_vvel(imin)=vvel_matrix(imin,itraj);
                min_asoc_vel(imin)=vel_matrix(imin,itraj);  
                mindistance=tdistance_matrix(imin,itraj); %Only a trajectory 
                %closer than the one I have found before could be the
                %continuation associated with minimun imin. 
            end
         end
       end
    end
      
    
%======================================================================
% THIRD STEP: NOW ADD ALL THE CONTINUATIONS TO THE TRAJECTORY STRUCTURE AND
% CREATE NEW TRAJECTORIES STARTING FROM THAT MIINMUNS THAT CANNOT BE
% ASSOCIATED WITH ANY SYSTEM.
%======================================================================   
    
    

    for imin=1:nmin %DO OVER CURRENT MINIMUNS.
          %A) THE SYSTEM IS WELL ASSOCIATED AND ALSO THE TRAJECTORY HAS A
          %POINT IN THE PREVIOUS TIME.
          if( min_asoc(imin) && MinStruct(itime).daten-trayectorias(min_asoc_traj(imin)).daten(end)==config.timefrec/24)
               ctraj=min_asoc_traj(imin);
               trajl=length(trayectorias(ctraj).minlat);
               trayectorias(ctraj).uvel(trajl+1)=min_asoc_uvel(imin);                               %#ok<AGROW>
               trayectorias(ctraj).vvel(trajl+1)=min_asoc_vvel(imin);                               %#ok<AGROW>
               trayectorias(ctraj).vel(trajl+1)=min_asoc_vel(imin);                                 %#ok<AGROW>
               trayectorias(ctraj).minlat(trajl+1)=MinStruct(itime).minlat(imin);              %#ok<AGROW>
               trayectorias(ctraj).minlon(trajl+1)=MinStruct(itime).minlon(imin);              %#ok<AGROW>
               trayectorias(ctraj).minarea(trajl+1)=MinStruct(itime).minarea(imin);            %#ok<AGROW>
               trayectorias(ctraj).minanomsis(trajl+1)=MinStruct(itime).minanomsis(imin);      %#ok<AGROW>
               trayectorias(ctraj).meananomsis(trajl+1)=MinStruct(itime).meananomsis(imin);    %#ok<AGROW>
               trayectorias(ctraj).daten(trajl+1)=MinStruct(itime).daten;                      %#ok<AGROW>
               if(MinStruct(itime).additionaldataflag)
                  trayectorias(ctraj).minadditionaldata(trajl+1,:)=MinStruct(itime).minadditionaldata(imin,:);   %#ok<AGROW>
                  trayectorias(ctraj).maxadditionaldata(trajl+1,:)=MinStruct(itime).maxadditionaldata(imin,:);   %#ok<AGROW>
                  trayectorias(ctraj).meanadditionaldata(trajl+1,:)=MinStruct(itime).meanadditionaldata(imin,:); %#ok<AGROW>
               end 
          
          
          %B) THE SYSTEM IS WELL ASSOCIATED BUT THE TRAJECTORY DOES NOT
          %HAVE A POINT IN THE PREVIOUS TIME.
          elseif( min_asoc(imin) && MinStruct(itime).daten-trayectorias(min_asoc_traj(imin)).daten(end)==2*config.timefrec/24)
          %IN THIS CASE FIRST WE WILL CREATE A FAKE POINT TAKING THE
          %AVERAGE BETWEEN THE POSITION OF THE TRAJECTORY AT TIME T AND AT
          %TIME T-2.
                   trajl=length(trayectorias(min_asoc_traj(imin)).minlat);
                   ctraj=min_asoc_traj(imin);
                   trayectorias(ctraj).minlat(trajl+1)=0.5*(trayectorias(ctraj).minlat(trajl)+MinStruct(itime).minlat(imin)); %#ok<AGROW>
                   %Average longitude taking into account possible
                   %discontinuities.
                   tmplon=[trayectorias(ctraj).minlon(trajl) MinStruct(itime).minlon(imin)];
                   %Compute mean of the longitudes taking into account the
                   %date change line.
                     meantmplon=mean_lon_fun(tmplon);
                     
                   trayectorias(ctraj).uvel(trajl+1)=min_asoc_uvel(imin);                               %#ok<AGROW>
                   trayectorias(ctraj).vvel(trajl+1)=min_asoc_vvel(imin);                               %#ok<AGROW>
                   trayectorias(ctraj).vel(trajl+1)=min_asoc_vel(imin);                                 %#ok<AGROW>
                   trayectorias(ctraj).minlon(trajl+1)=meantmplon;                                      %#ok<AGROW>
                   trayectorias(ctraj).minarea(trajl+1)=0.5*(trayectorias(ctraj).minarea(trajl)+MinStruct(itime).minarea(imin));             %#ok<AGROW>
                   trayectorias(ctraj).minanomsis(trajl+1)=0.5*(trayectorias(ctraj).minanomsis(trajl)+MinStruct(itime).minanomsis(imin));    %#ok<AGROW>
                   trayectorias(ctraj).meananomsis(trajl+1)=0.5*(trayectorias(ctraj).meananomsis(trajl)+MinStruct(itime).meananomsis(imin)); %#ok<AGROW>
                   trayectorias(ctraj).daten(trajl+1)= 0.5*(trayectorias(ctraj).daten(trajl)+MinStruct(itime).daten);                        %#ok<AGROW>
                 if(MinStruct(itime).additionaldataflag)
                   trayectorias(ctraj).minadditionaldata(trajl+1,:)=0.5*(trayectorias(ctraj).minadditionaldata(trajl,:)+MinStruct(itime).minadditionaldata(imin,:));    %#ok<AGROW>
                   trayectorias(ctraj).maxadditionaldata(trajl+1,:)=0.5*(trayectorias(ctraj).maxadditionaldata(trajl,:)+MinStruct(itime).maxadditionaldata(imin,:));    %#ok<AGROW>
                   trayectorias(ctraj).meanadditionaldata(trajl+1,:)=0.5*(trayectorias(ctraj).meanadditionaldata(trajl,:)+MinStruct(itime).meanadditionaldata(imin,:)); %#ok<AGROW>
                 end 
          
                 %NOW I CAN JUST ADD THE LAST TIME CORRESPONDING TO TIME ITIME.
                 trayectorias(ctraj).uvel(trajl+2)=min_asoc_uvel(imin);                        %#ok<AGROW>
                 trayectorias(ctraj).vvel(trajl+2)=min_asoc_vvel(imin);                        %#ok<AGROW>
                 trayectorias(ctraj).vel(trajl+2)=min_asoc_vel(imin);                          %#ok<AGROW>
                 trayectorias(ctraj).minlat(trajl+2)=MinStruct(itime).minlat(imin);            %#ok<AGROW>
                 trayectorias(ctraj).minlon(trajl+2)=MinStruct(itime).minlon(imin);            %#ok<AGROW>
                 trayectorias(ctraj).minarea(trajl+2)=MinStruct(itime).minarea(imin);          %#ok<AGROW>
                 trayectorias(ctraj).minanomsis(trajl+2)=MinStruct(itime).minanomsis(imin);    %#ok<AGROW>
                 trayectorias(ctraj).meananomsis(trajl+2)=MinStruct(itime).meananomsis(imin);  %#ok<AGROW>
                 trayectorias(ctraj).daten(trajl+2)=MinStruct(itime).daten;                    %#ok<AGROW>
                  if(MinStruct(itime).additionaldataflag)
                    trayectorias(ctraj).minadditionaldata(trajl+2,:)=MinStruct(itime).minadditionaldata(imin,:);     %#ok<AGROW>
                    trayectorias(ctraj).maxadditionaldata(trajl+2,:)=MinStruct(itime).maxadditionaldata(imin,:);     %#ok<AGROW>
                    trayectorias(ctraj).meanadditionaldata(trajl+2,:)=MinStruct(itime).meanadditionaldata(imin,:);   %#ok<AGROW>
                  end 
              
          elseif(~min_asoc(imin))
              
          %C) THE SYSTEM HAS NOT BEEN ASSOCIATED SO FAR, SO CREATE A NEW TRAJECTORY STARTING FROM THIS SYSTEM. 
          
           ntraj=ntraj+1; 
           ctraj=ntraj;
           trayectorias(ctraj).uvel=NaN;                                   %#ok<AGROW>
           trayectorias(ctraj).vvel=NaN;                                   %#ok<AGROW>
           trayectorias(ctraj).vel=NaN;                                    %#ok<AGROW>
           trayectorias(ntraj).minarea=MinStruct(itime).minarea(imin);     %#ok<AGROW>
           trayectorias(ntraj).minlat=MinStruct(itime).minlat(imin);       %#ok<AGROW>
           trayectorias(ntraj).minlon=MinStruct(itime).minlon(imin);       %#ok<AGROW>
           trayectorias(ntraj).minanomsis=MinStruct(itime).minanomsis(imin);   %#ok<AGROW>
           trayectorias(ntraj).meananomsis=MinStruct(itime).meananomsis(imin); %#ok<AGROW>
           trayectorias(ntraj).daten=MinStruct(itime).daten;               %#ok<AGROW>
          
           if(MinStruct(itime).additionaldataflag)
             trayectorias(ntraj).minadditionaldata=MinStruct(itime).minadditionaldata(imin,:); %#ok<AGROW>
             trayectorias(ntraj).maxadditionaldata=MinStruct(itime).maxadditionaldata(imin,:); %#ok<AGROW>
             trayectorias(ntraj).meanadditionaldata=MinStruct(itime).meanadditionaldata(imin,:); %#ok<AGROW>
           end   
          
              
              
              
              
          end
        
        
          
    end %END LOOP OVER MINIMUNS AT TIME ITIME
       
   

  
 end%End del if sobre si es el primer tiempo o no (tener en cuenta el else).
  
end  %END LOOP OVER TIME


%==========================================================================
% THE COMPUTATION OF THE TRAJECTORIES IS DONE SO FAR.
% THE FOLLOWING LINES CORRESPONDS TO DIFFERENT TEST TO FIND ANOMALOUS
% BEHAVIOUR OF THE TRAYECTORIES THAT CANNOT BE DETECTED DURING THE
% TRAJECTORY COMPUTATION STEP.
%==========================================================================



%==========================================================================
%  THIS SECTION OF THE CODE DEALS WITH DISCONTIUITIES OF TYPE "A" . THIS
%  MEANS DISCONTINUITIES WHERE THE TRAYECTORY APPARTS FROM ITS PREVIOUS
%  BEHAVIOUR AT ONE POINT BUT THEN RETURNS BACK. IN THIS CASE THE
%  TRAJECTORY IS KEPT AND THE SUSPICIOUS POINT IS REMOVED AND REPLACED BY
%  THE AVERAGE OF THE PREVIOUS AND NEXT POINTS IN THE TRAJECTORY.
%  THE PROCEDURE CAN BE REPETEAD SEVERAL TIMES BUT USUALLY ONLY ONE PASS OF
%  THE FILTER WILL BE ENOUGTH TO CORRECT THE MOST IMPORTANT OUTLIERS.
%==========================================================================
%  THE FILTER CHECKS FOR THE ANGLE BETWEEN THE VELOCITY BETWEEN T-1 AND T
%  AND T AND T+1. IF THE ANGLE BETWEEN THIS TWO VELOCITIES APPROACHES PI
%  THEN THE POINT IS CONSIDERED A SUSPICIOUS POINT AND THE BAD POINT IS
%  REPLACED BY THE AVERAGED OF THE PREVIOUS AND THE NEXT POINTS. THEN THE
%  VELOCITIES ARE RECOMPUTED (IN CASE THAT THE FILTER HAS TO BE APPLIED
%  AGAIN OR FOR FURTHER DETECTION OF ANOMALIES IN THE TRAYECTORIES).

if(DetectIsolatedBadPoints)
   AngleCriteria=45;   %If the angle of the trajectory turn at the suspicius point
                       %is less than this and the associated velocities are
                       %big enought then the point will be eliminated from
                       %the trajectory.
   NumberFilterPases=1;%How many times does the filtering process be repeated.
                       
   %LETS APPLY AN SMOOTHER TO THE TRAYECTORY.
for ifilter=1:NumberFilterPases   
   for itraj=1:ntraj
      if(length(trayectorias(itraj).minlat) > 3)

         for ii=2:(length(trayectorias(itraj).minlat)-1)
             %Compute velociti angle between ii-1,ii and ii,ii+1
             ut1=trayectorias(itraj).uvel(ii);
             vt1=trayectorias(itraj).vvel(ii);
             ut2=trayectorias(itraj).uvel(ii+1);
             vt2=trayectorias(itraj).vvel(ii+1);
             vtott1=sqrt(ut1^2+vt1^2);
             vtott2=sqrt(ut2^2+vt2^2);
             innerprod=(ut1*ut2+vt1*vt2);
             argument=innerprod/(vtott1*vtott2);
             if(argument > 1)
                 argument=1;
             elseif(argument < -1)
                 argument=-1;
             end
             angle=acos(argument);
             angle=abs(angle-pi);

              if(angle < (AngleCriteria*pi/180) )%& vtott1 > 5 & vtott2 > 5 ) %& vtott1 > 10 & vtott2 > 10);
              %In this condition we can also consider the velocities involved in the displacement away and 
              %towards the trajectories (would it be necesary to do so?)
%                %In this case the point is quite under a cloud.
%                %We will replace this point by the average between point
%                %ii-1 and point ii.
                 %fprintf('I HAVE FOUND A DISCONTINUITY TYPE A, LETS CORRECT IT\n')
                 trayectorias(itraj).minlat(ii)=0.5*(trayectorias(itraj).minlat(ii-1)+trayectorias(itraj).minlat(ii+1)); %#ok<AGROW>
                 tmplon=[trayectorias(itraj).minlon(ii-1) trayectorias(itraj).minlon(ii+1)];
                 trayectorias(itraj).minlon(ii)=mean_lon_fun(tmplon); %#ok<AGROW>
                 %Recompute associated trayectory velocities from ii-1 to
                 %ii and from ii to ii+1;
                 deltat=config.timefrec*3600;
                 [trayectorias(itraj).uvel(ii) trayectorias(itraj).vvel(ii)]=...
                             compute_uv_fun(trayectorias(itraj).minlon(ii-1),trayectorias(itraj).minlat(ii-1),...
                                            trayectorias(itraj).minlon(ii)  ,trayectorias(itraj).minlat(ii),deltat); %#ok<AGROW>
                 [trayectorias(itraj).uvel(ii+1) trayectorias(itraj).vvel(ii+1)]=...
                             compute_uv_fun(trayectorias(itraj).minlon(ii),trayectorias(itraj).minlat(ii),...
                                            trayectorias(itraj).minlon(ii+1)  ,trayectorias(itraj).minlat(ii+1),deltat); %#ok<AGROW>
%                %I only change the possition of the point but retain all the
%                %information about the system (since most of the time this
%                %information is correct but the discontinuity is produced by
%                %a sudden change in the minimun posittion.
              end
         end
          
      end
   end
end
end

%==========================================================================
% THIS SECTION OF THE CODE LEADS WITH DISCONTINUITIES OF TYPE "B"
% IF A TRAJECTORY EXPERIMENTS A SUDDEN CHANGE IN ITS VELOCITY (USUALLY
% SIMULTANEOUSLY AT U AND V) THEN IT MIGHT BE INDICATING THAT THE SYSTEM
% MINIMUN POSITION HAS SUFFERED A CHANGE ASSOCIATED WITH A REDEVELOPMENT OF
% THE SYSTEM. UNDER THIS CIRCUMSTANCES THE TRAJECTORY WILL BE SPLITED INTO
% TWO TRAJECTORIES (IF THE SPLIT WAS NOT CORRECT, THE GROUPING ALGORITHM
% WILL CONSIDER THIS TWO TRAJECTORIES AS THE SAME ONE AGAIN).
%==========================================================================

if(IfContinuityCheck)
ntraj=size(trayectorias,2);
tot_traj=ntraj;
for itraj=1:ntraj
   if( length(trayectorias(itraj).minlat) > 4)
     v=trayectorias(itraj).vvel;
     u=trayectorias(itraj).uvel;
     continuidad=zeros(size(u));
     vmedia=zeros(size(u));
     for kk=3:length(trayectorias(itraj).minlat)-3
       %Calculo el indice de continuidad basado exclusivamente en el
       %criterio de velocidad. Esto evita que algunas trayectorias que
       %corresponden a sistemas que son sucesion o redesarrollos de otros
       %se mezclen entre grupos.
         %Uso las velocidades no filtradas para evitar que el suavizado
         %pueda moderar esta situacion.

         %continuidad(kk)=abs( u(kk+1) - u(kk) ) + abs( u(kk+2) - u(kk+1) ) + ...
         %            abs( v(kk+1) - v(kk) ) + abs( v(kk+2) - v(kk+1) );
         continuidad(kk)=abs( 0.5*(u(kk-1)+u(kk)) - 0.5*(u(kk+1)+u(kk+2) ) )+ ...
                         abs( 0.5*(v(kk-1)+v(kk)) - 0.5*(v(kk+1)+v(kk+2) ) );
         %vmedia(kk)=max([abs(0.5*(u(kk-1)+u(kk))) abs(0.5*(v(kk-1)+v(kk))) ]);            
                     
     end
     %vmedia(vmedia < 1)=1;
     %Busco el punto donde continuidad es maximo y si esta por encima del
     %umbral rompo la trayectoria en ese punto.
         [max_continuidad mck]=max(continuidad);
         if(max_continuidad > TrContinuity )    
             %Si esto sucede, entonces la trayectoria debe romperse en el
             %punto mck, es decir el punto mck+1 debe ser el inicio de una
             %nueva trayectoria.
            
         %Si continuidad es mayor que el umbral, entonces la trayectoria
         %tiene una discontinuidad fea en la velocidad.
         %Vamos a dividirla en 2 trayectorias.
         tot_traj=tot_traj+1;
         %A partir del punto kk todo lo que sigue va a ser asignado a una nueva trayectoria.
         fprintf('Rompo la trayectoria %f y creo la trayectoria %f \n',itraj,tot_traj);
         
             trayectorias(tot_traj).minarea=trayectorias(itraj).minarea(mck+1:end); %#ok<AGROW>
             trayectorias(itraj).minarea(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).minlat=trayectorias(itraj).minlat(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).minlat(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).minlon=trayectorias(itraj).minlon(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).minlon(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).uvel=trayectorias(itraj).uvel(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).uvel(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).vvel=trayectorias(itraj).vvel(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).vvel(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).vel=trayectorias(itraj).vel(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).vel(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).minanomsis=trayectorias(itraj).minanomsis(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).minanomsis(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).meananomsis=trayectorias(itraj).meananomsis(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).meananomsis(mck+1:end)=[];%#ok<AGROW>
             trayectorias(tot_traj).daten=trayectorias(itraj).daten(mck+1:end);%#ok<AGROW>
             trayectorias(itraj).daten(mck+1:end)=[];%#ok<AGROW>
             
             %Split additional fields if present.
             
             if(MinStruct(1).additionaldataflag)
               trayectorias(tot_traj).minadditionaldata=trayectorias(itraj).minadditionaldata(mck+1:end);   %#ok<AGROW>
               trayectorias(itraj).minadditionaldata(mck+1:end)=[];                                         %#ok<AGROW>
               trayectorias(tot_traj).maxadditionaldata=trayectorias(itraj).maxadditionaldata(mck+1:end);   %#ok<AGROW>
               trayectorias(itraj).maxadditionaldata(mck+1:end)=[];                                         %#ok<AGROW>
               trayectorias(tot_traj).meanadditionaldata=trayectorias(itraj).meanadditionaldata(mck+1:end); %#ok<AGROW>
               trayectorias(itraj).meanadditionaldata(mck+1:end)=[];                                        %#ok<AGROW>
             end   
                
         end
   end
end

end %End for the if wether to do or not the continuity check.


%==========================================================================
% THIS SECTION OF THE CODES TAKES CASES WHERE A REDEVELOPMENT OF THE SYSTEM
% OCCOURS AND THE TWO TRAJECTORIES OVERLAP DURING A SINGLE TIME STEP. THIS
% MIGHT PRODUCE PROBLEMS WHEN TRYING TO ASSOCIATE THE TRAJECTORIES DURING
% THE GROUPING STAGE. IN THIS CASE WE WON'T MERGE THE TRAJECTORIES, HOWEVER
% WE WILL AVERAGE THE POSSITION OF THE MINIMUN AT THE TIME WHERE BOTH
% TRAJECTORIES OVERLAP AND WE WILL ASSIGN THIS TO THE ENDING TRAJECTORY. 
%==============================================================

if(IfRedevelopmentSuperpossitionCheck)
ntraj=size(trayectorias,2);
for itraj=1:ntraj
    %For each trajectory I will check if for the end of this trajectory
    %there is a close trajectory which is starting at the same time. 


    for jtraj=1:ntraj
        if( length(trayectorias(itraj).minlat) > 1 && length(trayectorias(jtraj).minlat) > 1)
        %First check if the end of trajectory itraj is at the same time of
        %the beginning of trajectory jtraj.
        if(~isempty(trayectorias(itraj).daten))
        
        if(~isempty(trayectorias(jtraj).daten) &&  trayectorias(itraj).daten(end) == trayectorias(jtraj).daten(1))
            %Now check if the last point of itraj is close to the first
            %point of jtraj.
           
            dist_min=distll_fun(trayectorias(itraj).minlon(end)  ,trayectorias(itraj).minlat(end),...
                                trayectorias(jtraj).minlon(1)    ,trayectorias(jtraj).minlat(1));
            if(dist_min <= MaxTravelDistance ) %I use the same distance criterion as for system propagation.
               %If they are close, lets compute the mean distance.
               mean_lat=0.5*(trayectorias(itraj).minlat(end)+trayectorias(jtraj).minlat(1));
               mean_lon=mean_lon_fun([trayectorias(itraj).minlon(end) trayectorias(jtraj).minlon(1)]);
               %Lets modify the last point of itraj
               trayectorias(itraj).minlat(end)=mean_lat; %#ok<AGROW>
               trayectorias(itraj).minlon(end)=mean_lon; %#ok<AGROW>
               %Lets sum the systems area.
               trayectorias(itraj).minarea(end)=trayectorias(itraj).minarea(end)+trayectorias(jtraj).minarea(1);
               
               %Do not recompute U, V since this change is artificial and
               %U, and V will be recomputed later when the filter is
               %applied.
               
               %Now eliminate the first point of the new trayectory. The
               %idea of eliminate this point instead of modifying it is
               %that further computations rely more on the first points of
               %the trayectory so it is better not to introduce noise at
               %the beginning of the trayectory.
               trayectorias(jtraj).uvel(1)=[];                                   %#ok<AGROW>
               trayectorias(jtraj).vvel(1)=[];                                   %#ok<AGROW>
               trayectorias(jtraj).vel(1)=[];                                    %#ok<AGROW>
               trayectorias(jtraj).minarea(1)=[];                                %#ok<AGROW>
               trayectorias(jtraj).minlat(1)=[];                                 %#ok<AGROW>
               trayectorias(jtraj).minlon(1)=[];                                 %#ok<AGROW>
               trayectorias(jtraj).minanomsis(1)=[];                             %#ok<AGROW>
               trayectorias(jtraj).meananomsis(1)=[];                            %#ok<AGROW>
               trayectorias(jtraj).daten(1)=[];                                  %#ok<AGROW>
          
           if(MinStruct(itime).additionaldataflag)
               trayectorias(jtraj).minadditionaldata(1)=[];                      %#ok<AGROW>
               trayectorias(jtraj).maxadditionaldata(1)=[];                      %#ok<AGROW>
               trayectorias(jtraj).meanadditionaldata(1)=[];                     %#ok<AGROW>
           end
           
           
           
            end
            end
 
        end
  
    end
    end
end

end %End for the if wether to do or not the continuity check.





%==========================================================================
%  THIS SECTION OF THE CODE DEALS WITH THE FUSSION OF SHORT TRAYECTORIES
%  WITH LONGER TRAYECTORIES IN THE NEIGHBOURHOOD. 
%  IF A TRAYECTORY HAS ONLY ONE TIME, AND THERE IS A LONG TRAYECTORY WHICH
%  IS CLOSE TO THIS AT THE SAME TIME, THEN THE AREA ASSOCIATED WITH THE
%  SHORT TRAYECTORY IS ADDED TO THE AREA ASSOCIATED WITH THE LONG
%  TRAYECTORY.
%==========================================================================

if(AssociateShortTrajectories)
ntraj=size(trayectorias,2);
      for itraj=1:ntraj
          if(length(trayectorias(itraj).minlat) ==1) %Then this is a short trajectory.
             time_short_traj=trayectorias(itraj).daten(1); 
             %Now search throught the other trajectories to see if there is a close trajectory at the selected time.    
             for ii=1:ntraj
                 if( ii ~= itraj && sum(trayectorias(ii).daten==time_short_traj) > 0)
                     itime=find(trayectorias(ii).daten==time_short_traj);
                     %Compute the distance between the two trayectories at
                     %the time of the short trayectory appearence.
                     dist=distll_fun(trayectorias(ii).minlon(itime),trayectorias(ii).minlat(itime) ...
                                    ,trayectorias(itraj).minlon(1),trayectorias(itraj).minlat(1));
                                
                     if(dist <=   MaxTravelDistance) %Then associate the two systems.
                     %only the system properties will be updated, the
                     %position and the speed of the system will remain
                     %unchanged.
                     

                     area1=trayectorias(ii).minarea(itime);
                     area2=trayectorias(itraj).minarea(1);
                     trayectorias(ii).minarea(itime)=area1+area2;     %#ok<AGROW>
                     anom1=trayectorias(ii).meananomsis(itime);
                     anom2=trayectorias(itraj).meananomsis(1);
                     trayectorias(ii).meananomsis(itime)=(area1*anom1+area2*anom2)/(area1+area2); %#ok<AGROW>

                     if(MinStruct(1).additionaldataflag)
                      trayectorias(ii).minadditionaldata(itime)=min([trayectorias(ii).minadditionaldata(itime) trayectorias(itraj).minadditionaldata(1)]); %#ok<AGROW>
                      trayectorias(ii).maxadditionaldata(itime)=min([trayectorias(ii).maxadditionaldata(itime) trayectorias(itraj).maxadditionaldata(1)]); %#ok<AGROW>
                      mean1=trayectorias(ii).meanadditionaldata(itime);
                      mean2=trayectorias(itraj).meanadditionaldata(1);
                      trayectorias(ii).meanadditionaldata(itime)=(area1*mean1+area2*mean2)/(area1+area2); %#ok<AGROW>
                     end            
                     end
                 
                 end
             end
          end
      end
end


%==========================================================================
% THIS SECTION OF THE CODES APPLIES A FILTER TO THE POSSITION OF THE
% SYSTEM AT SEVERAL TIMES. THIS GENERATES A SMOOTHER TRAYECTORY.
%==========================================================================

if(SmoothTrajectory)
ntraj=size(trayectorias,2);
   
   %LETS APPLY AN SMOOTHER TO THE TRAYECTORY.
   for itraj=1:ntraj
      
         trayectorias(itraj).minlatf=trayectorias(itraj).minlat;           %#ok<AGROW>
         trayectorias(itraj).minlonf=trayectorias(itraj).minlon;           %#ok<AGROW>
         trayectorias(itraj).uvelf=trayectorias(itraj).uvel;               %#ok<AGROW>
         trayectorias(itraj).vvelf=trayectorias(itraj).vvel;               %#ok<AGROW>
         trayectorias(itraj).velf=trayectorias(itraj).vel;                 %#ok<AGROW>
         trayectorias(itraj).minareaf=trayectorias(itraj).minarea;         %#ok<AGROW>
         trayectorias(itraj).minlatf=trayectorias(itraj).minlat;           %#ok<AGROW>
         trayectorias(itraj).minlonf=trayectorias(itraj).minlon;           %#ok<AGROW>
         trayectorias(itraj).minanomsisf=trayectorias(itraj).minanomsis;   %#ok<AGROW>
         trayectorias(itraj).meananomsisf=trayectorias(itraj).meananomsis; %#ok<AGROW>
         
         if(MinStruct(1).additionaldataflag)
             trayectorias(itraj).minadditionaldataf=trayectorias(itraj).minadditionaldata;   %#ok<AGROW>
             trayectorias(itraj).maxadditionaldataf=trayectorias(itraj).maxadditionaldata;   %#ok<AGROW>
             trayectorias(itraj).meanadditionaldataf=trayectorias(itraj).meanadditionaldata; %#ok<AGROW>
         end    
   end
  
   
  for jj=1:SmoothSteps
      for itraj=1:ntraj
      if(length(trayectorias(itraj).minlat) > 3)
         tmplat=trayectorias(itraj).minlatf;
         tmplon=trayectorias(itraj).minlonf;
         tmparea=trayectorias(itraj).minareaf;
         tmpminanom=trayectorias(itraj).minanomsisf;
         tmpmeananom=trayectorias(itraj).meananomsisf;
            if(MinStruct(1).additionaldataflag)
                tmpaddmin=trayectorias(itraj).minadditionaldata;
                tmpaddmax=trayectorias(itraj).maxadditionaldata;
                tmpaddmean=trayectorias(itraj).meanadditionaldata;
            end
         
         for kk=2:length(trayectorias(itraj).minlat)-1 
         %Recompute smoothed trayectory position.
         trayectorias(itraj).minlatf(kk)=mean(tmplat(kk-1:kk+1));          %#ok<AGROW>
         trayectorias(itraj).minlonf(kk)=mean_lon_fun(tmplon(kk-1:kk+1));  %#ok<AGROW>
         %Recompute smoothed system velocity.
         [trayectorias(itraj).uvelf(kk) trayectorias(itraj).vvelf(kk) trayectorias(itraj).velf(kk)]=...
         compute_uv_fun(trayectorias(itraj).minlonf(kk-1),trayectorias(itraj).minlatf(kk-1),        ...
         trayectorias(itraj).minlonf(kk)  ,trayectorias(itraj).minlatf(kk),deltat);    %#ok<AGROW>

         trayectorias(itraj).minareaf(kk)=mean(tmparea(kk-1:kk+1));                    %#ok<AGROW>
         trayectorias(itraj).minanomsisf(kk)=mean(tmpminanom(kk-1:kk+1));              %#ok<AGROW>
         trayectorias(itraj).meananomsisf(kk)=mean(tmpmeananom(kk-1:kk+1));            %#ok<AGROW>
         
           if(MinStruct(1).additionaldataflag)
             trayectorias(itraj).minadditionaldataf(kk)=mean(tmpaddmin(kk-1:kk+1));    %#ok<AGROW>
             trayectorias(itraj).maxadditionaldataf(kk)=mean(tmpaddmax(kk-1:kk+1));    %#ok<AGROW>
             trayectorias(itraj).meanadditionaldataf(kk)=mean(tmpaddmean(kk-1:kk+1));  %#ok<AGROW>
           end 
     
         end
         [trayectorias(itraj).uvelf(end) trayectorias(itraj).vvelf(end) trayectorias(itraj).velf(end)]=...
         compute_uv_fun(trayectorias(itraj).minlonf(end-1),trayectorias(itraj).minlatf(end-1),...
         trayectorias(itraj).minlonf(end)  ,trayectorias(itraj).minlatf(end),deltat); %#ok<AGROW>
         trayectorias(itraj).uvelf(1)=NaN;                                 %#ok<AGROW>
         trayectorias(itraj).vvelf(1)=NaN;                                 %#ok<AGROW>
         trayectorias(itraj).velf(1)=NaN;                                  %#ok<AGROW>
      end
   end
  end
end



%==========================================================================
% THIS PART OF THE CODE DOES SOME ADDITIONAL COMPUTATION AS FORECAST LEAD
% TIME (IN CASE OF FORECAST MODE), TRAJECTORY LENGTH, START DATE, END DATE
%==========================================================================

ntraj=size(trayectorias,2);
   for itraj=1:ntraj
      %Compute length.
      trayectorias(itraj).length=length(trayectorias(itraj).minlat);       %#ok<AGROW>
      trayectorias(itraj).startdate=trayectorias(itraj).daten(1);          %#ok<AGROW>
      trayectorias(itraj).enddate=trayectorias(itraj).daten(end);          %#ok<AGROW>
      %Forecast lead time in hours (in case of analysis, time since
      %trajectory computation initialization);
      trayectorias(itraj).leadtime=(trayectorias(itraj).daten-MinStruct(1).daten)*24;      %#ok<AGROW>
   end

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
















