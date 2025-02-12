clear all
close all

%==========================================================================  
%Leemos los datos.
%==========================================================================  

fid=fopen('input.txt');

i=1;
  while 1
    tline = fgetl(fid);
   
    if ~ischar(tline), break, end
    %disp(tline)
    
    tmp  =str2num(tline(26:38));
    if(isempty(tmp))
        p(i)=NaN;
    else
        p(i)=tmp;
    end
    
    tmp   =str2num(tline(39:48));
    if(isempty(tmp))
        t(i)=NaN;
    else
        t(i)=tmp;
    end
    
    tmp  =str2num(tline(49:58));
    
    if(isempty(tmp))
        td(i)=NaN;
    else
        td(i)=tmp;
    end
    
    tmp  =str2num(tline(59:70));
    
    if(isempty(tmp))
        vel(i)=NaN;
    else
        vel(i)=tmp;
    end
    
    tmp =str2num(tline(71:79));
    
    if(isempty(tmp))
        dir(i)=NaN;
    else
        dir(i)=tmp;
    end


    if(i==1 || p(i) < p(i-1))
     i=i+1;
    end
    

  end
  fclose(fid);
















