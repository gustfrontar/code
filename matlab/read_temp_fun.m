
function [t,td,p,vel,dir,id,fecha,hora]=read_temp_fun(filename)

t=NaN;
td=NaN;
p=NaN;
dir=NaN;
vel=NaN;
fecha=NaN;
id=NaN;
hora=NaN;

mandatory_levels=[1000 950 850 700 500 300 250 100];
%==========================================================================  
%Leemos los datos.
%==========================================================================  

fid=fopen(filename);

i=1;
  while 1
    tline = fgetl(fid);
    

    if ~ischar(tline), break, end
    
    
        if(i == 1)
            
           id =str2num(tline(1:10));
           fecha=str2num(tline(11:20));
           hora=str2num(tline(21:24));
               
           
        end
    
    %disp(tline)
    
    tmp  =str2num(tline(25:32));
    if(isempty(tmp))
        p(i)=NaN;
    else
        p(i)=tmp;
    end
    
    tmp   =str2num(tline(33:39));
    if(isempty(tmp))
        t(i)=NaN;
    else
        t(i)=tmp;
    end
    
    tmp  =str2num(tline(40:45));
    
    if(isempty(tmp))
        td(i)=NaN;
    else
        td(i)=tmp;
    end
    
    tmp  =str2num(tline(46:52));
    
    if(isempty(tmp))
        dir(i)=NaN;
    else
        dir(i)=tmp;
    end
    
    tmp =str2num(tline(53:56));
    
    if(isempty(tmp))
        vel(i)=NaN;
    else
        vel(i)=tmp;
    end
    

    for jj=1:length(mandatory_levels)
    if(p(i) < mandatory_levels(jj) && p(i-1) > mandatory_levels(jj) )
       p(i+1)=p(i);
       t(i+1)=t(i);
       td(i+1)=td(i);
       dir(i+1)=dir(i);
       vel(i+1)=vel(i);
       
       p(i)=mandatory_levels(jj);
       t(i)=NaN;
       td(i)=NaN;
       dir(i)=NaN;
       vel(i)=NaN;
        
       i=i+1;
    end
    end
    
    
    if(i==1 || p(i) < p(i-1))
     i=i+1;
    end

  end
  fclose(fid);
















