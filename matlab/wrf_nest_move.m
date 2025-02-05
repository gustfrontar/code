
moving_grid=4;
num_moves=0;  %Initialize number of moves.
move_id_grid=[];  %Which grid si going to be moved.
move_interval=[]; %Movement times
move_cd_x=[];
move_cd_y=[];
dt=5;            %Dt in seconds.
dx=1000;         %Dx in meters

%minutes since run start 	x-index (m)		y-index	(m)	
data=[
0   14000       50000;
60  14000       50000;     
120	79000		69000;		
130	88000		70000;		
140	99000		73000;		
150	110000		74000;		
160	123000		81000];	

time=data(:,1);
posx=data(:,2);
posy=data(:,3);


despx=0;
despy=0;
total_pos_x=posx(1);
total_pos_y=posy(1);

%Loop over defined time intervals.
for ii=1:length(time)-1
   
    init_time=time(ii)*60;
    end_time=time(ii+1)*60;
    
    vel_x=(posx(ii+1)-posx(ii))/(end_time-init_time);
    vel_y=(posy(ii+1)-posy(ii))/(end_time-init_time);
    
    for jj=0:dt:end_time-init_time
        despx=despx+vel_x*dt;
        despy=despy+vel_y*dt;
       
        if(despx > dx & despy > dx )
           move_interval=[move_interval (jj+init_time)/60]; %#ok<AGROW>
           move_cd_x=[move_cd_x 1*sign(despx)];
           move_cd_y=[move_cd_y 1*sign(despy)];
           despx=despx-dx;
           despy=despy-dx; 
           total_pos_x=[total_pos_x total_pos_x(end)+dx];
           total_pos_y=[total_pos_y total_pos_y(end)+dx];
           move_id_grid=[move_id_grid moving_grid];
        end
        if(despx > dx & despy < dx )       
           move_interval=[move_interval (jj+init_time)/60]; %#ok<AGROW>
           move_cd_x=[move_cd_x 1*sign(despx)];
           move_cd_y=[move_cd_y 0];
           despx=despx-dx;
           total_pos_x=[total_pos_x total_pos_x(end)+dx];
           total_pos_y=[total_pos_y total_pos_y(end)];
           move_id_grid=[move_id_grid moving_grid];
        end
        if(despx < dx & despy > dx )
           move_interval=[move_interval (jj+init_time)/60]; %#ok<AGROW>
           move_cd_y=[move_cd_y 1*sign(despy)];
           move_cd_x=[move_cd_x 0];
           despy=despy-dx; 
           total_pos_x=[total_pos_x total_pos_x(end)];
           total_pos_y=[total_pos_y total_pos_y(end)+dx];
           move_id_grid=[move_id_grid moving_grid];
        end

    end
    
end

num_moves=length(move_cd_x)








