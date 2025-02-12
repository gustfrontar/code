function [system_struct mascaraid] = cluster_fun(input_mask,input_field,lon,lat);

[ny nx]=size(input_mask);

%Identify cluster in the mask.

sistemaid=1;
mascaraid=zeros(size(input_mask));

for ii=2:nx-1
    for jj=2:ny-1
          
        if ( input_mask(jj,ii) )
        %This point belongs to a system.
            for kkx=ii-1:ii
                for kky=jj-1:jj
                    if( kky ~= jj | kkx~=ii)
                       if(mascaraid(kky,kkx) > 0)
                          %There is a contiguos system.
                          if(mascaraid(jj,ii)==0)
                              mascaraid(jj,ii)=mascaraid(kky,kkx);
                              %mascaraid(kky,kkx)
                          end
                          if(mascaraid(jj,ii) > 0 & mascaraid(jj,ii) ~= mascaraid(kky,kkx));
                          %Conflict two different systems!
                              mascaraid(mascaraid==mascaraid(jj,ii))=mascaraid(kky,kkx);
                          %Solved!
                          end
                            
                       end
                    end
                    
                end
            end
            
            if(mascaraid(jj,ii) == 0)
            %By now if the system has not been identified with a previous
            %system then it might be a new system.
            mascaraid(jj,ii)=sistemaid;
            sistemaid=sistemaid+1 ;
            end
        end
        
        
    end
end

%Reasignamos los id de systema sequencialmente.
tmp=unique(mascaraid);
system_struct.nsistemas=length(tmp)-1;
for ii=2:length(tmp)
   mascaraid(mascaraid==tmp(ii))=ii-1; 
end


%GET SYSTEM CHARACTERISTICS.


for ii=1:system_struct.nsistemas
    
   system_struct.systems(ii).id=ii;
   current_system_mask=(mascaraid==ii);
   current_system_pp=input_field(current_system_mask);
   
   system_struct.systems(ii).meanpp=mean(current_system_pp);
   system_struct.systems(ii).maxpp=max(current_system_pp);
   system_struct.systems(ii).gridsize=sum(sum(current_system_mask));
   system_struct.systems(ii).centroidlon=mean(lon(current_system_mask));
   system_struct.systems(ii).centroidlat=mean(lat(current_system_mask));
    
   system_struct.systems(ii).wcentroidlon=mean(current_system_pp.*lon(current_system_mask))/mean(current_system_pp);
   system_struct.systems(ii).wcentroidlat=mean(current_system_pp.*lat(current_system_mask))/mean(current_system_pp);
  
   %System aspect ration and orientation (based on EOF analysis).
   latloncovar=cov([lon(current_system_mask) lat(current_system_mask)]);
   [U,D,V] = svds(latloncovar,2);
   F  = U*D^(0.5);
    if(F(1,1) > 0) %Adjust orientation 
        F(1,1)=-F(1,1);
        F(2,1)=-F(2,1);
    end
    if(F(1,2) < 0) %Adjust orientation 
        F(1,2)=-F(1,2);
        F(2,2)=-F(2,2);
    end
   system_struct.systems(ii).eof=F;
   axis=[sqrt(F(1,1)^2+F(2,1)^2) sqrt(F(1,2)^2+F(2,2)^2)];
   system_struct.systems(ii).axis=axis;
   system_struct.systems(ii).aspectratio=axis(1)/axis(2);
   orientation=atan(F(1,2)/F(1,1));
   if(orientation < 0)
       orientation=2*pi+orientation;
   end
   system_struct.systems(ii).orientation=orientation;
   
end






