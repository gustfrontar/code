function MinStruct=split_check_fun(config)



ini_date_num=datenum(config.date_ini,'yyyymmddHH');
end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;
%This function will chek for premature splitings.

while ( cur_date_num <= end_date_num )

    %READ DATA...
    MinFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '.bin'];
    
    if(count < 3)
        Minimos(count)=read_min_fun(MinFileName);
    end
    if(count >= 3)
        Minimos(3)=read_min_fun(MinFileName);
        
        %Check for splits.
        %Here I have to check if any of the systems at time 2 overlap with
        %any of the systems at time 1 and 3. Then I will check if 2 or more
        %systems at time 2 overlap with the same systems at time 1 and 3.
        %If so this means that a premature sppliting has occour and I will
        %merge those systems.
        
        %This array will kept the information about the overlaping between
        %minimuns in time 2 and minimuns in time 3 and 1.
        AssociatedMinimun=zeros(2,Minimos(2).nminimos);
        AssociatedAmount=zeros(2,Minimos(2).nminimos);
        
        for ii=1:Minimos(2).nminimos
            
            %Check the size of the intersection between system ii and all
            %the systems present in time 3.
            for jj=1:Minimos(3).nminimos
                overlap=length(intersect(Minimos(2).minindex{ii},Minimos(3).minindex{jj}));
                
                if(overlap/length(Minimos(2).minindex{ii}) > AssociatedAmount(2,ii))  
                    AssociatedAmount(2,ii)=overlap/length(Minimos(2).minindex{ii});
                    if(AssociatedAmount(2,ii) > config.umbral_overlap)
                    AssociatedMinimun(2,ii)=jj; %Minimun ii is associated with minimun jj at time 3.
                    end
                end
            end
            %Same stuff for time 1
            for jj=1:Minimos(1).nminimos
                overlap=length(intersect(Minimos(2).minindex{ii},Minimos(1).minindex{jj}));
                if(overlap/length(Minimos(2).minindex{ii}) > AssociatedAmount(1,ii))  
                    AssociatedAmount(1,ii)=overlap/length(Minimos(2).minindex{ii});
                    if(AssociatedAmount(1,ii) > config.umbral_overlap)
                    AssociatedMinimun(1,ii)=jj; %Minimun ii is associated with minimun jj at time 3.
                    end
                end
            end
        end
        %AssociatedAmount
        %AssociatedMinimun
        %Now we have to check if two or more minimuns has the same
        %associated minimuns.
        for ii=1:Minimos(2).nminimos
        tmp=find(AssociatedMinimun(1,:)==AssociatedMinimun(1,ii) & AssociatedMinimun(2,:)==AssociatedMinimun(2,ii));
            if(length(tmp) > 1 & AssociatedMinimun(1,:) > 0 & AssociatedMinimun(2,:) > 0)
                %There are at least one minimun that has the same relatives!
                fprintf('SPLIT FOUND\n')
                        tmp
                        AssociatedAmount
                        AssociatedMinimun
            
            end
            
        end
        
        
        
        
        %Save some fields to the MinStruct
        
        MinStruct(count-1).nf=Minimos(2).nf; %#ok<AGROW>
        MinStruct(count-1).nminimos=Minimos(2).nminimos; %#ok<AGROW>
        MinStruct(count-1).minlat=Minimos(2).minlatint; %#ok<AGROW>
        MinStruct(count-1).minlon=Minimos(2).minlonint; %#ok<AGROW>
        MinStruct(count-1).additionaldataflag=Minimos(2).additionaldataflag; %#ok<AGROW>
        MinStruct(count-1).minarea=Minimos(2).minarea; %#ok<AGROW>
        MinStruct(count-1).minanomsis=Minimos(2).minanomsis; %#ok<AGROW>
        MinStruct(count-1).meananomsis=Minimos(2).meananomsis; %#ok<AGROW>

        if(MinStruct(count-1).additionaldataflag)
          MinStruct(count-1).minanomsis=Minimos(2).minanomsis; %#ok<AGROW>
          MinStruct(count-1).maxanomsis=Minimos(2).maxanomsis; %#ok<AGROW>
          MinStruct(count-1).meananomsis=Minimos(2).meananomsis; %#ok<AGROW>
        end 
        
        %Shift Minimos
        Minimos(1)=Minimos(2);
        Minimos(2)=Minimos(3);
    end
    
    
    if(count == 1)
        %No split check can be performed in this case.
        %The first time is copyed as is.
        MinStruct(1).nf=Minimos(1).nf;
        MinStruct(1).nminimos=Minimos(1).nminimos;
        MinStruct(1).minlat=Minimos(1).minlatint;
        MinStruct(1).minlon=Minimos(1).minlonint;
        MinStruct(1).additionaldataflag=Minimos(1).additionaldataflag;
        MinStruct(1).minarea=Minimos(1).minarea;
        MinStruct(1).minanomsis=Minimos(1).minanomsis;
        MinStruct(1).meananomsis=Minimos(1).meananomsis;
        if(MinStruct(1).additionaldataflag)
          MinStruct(1).minanomsis=Minimos(1).minanomsis;
          MinStruct(1).maxanomsis=Minimos(1).maxanomsis;
          MinStruct(1).meananomsis=Minimos(1).meananomsis;
        end 
    end
    if(cur_date_num==end_date_num)
        %No split check can be performed in this case.
        %The last time is copyed as is.
        MinStruct(count).nf=Minimos(3).nf; %#ok<AGROW>
        MinStruct(count).nminimos=Minimos(3).nminimos; %#ok<AGROW>
        MinStruct(count).minlat=Minimos(3).minlatint; %#ok<AGROW>
        MinStruct(count).minlon=Minimos(3).minlonint; %#ok<AGROW>
        MinStruct(count).additionaldataflag=Minimos(3).additionaldataflag; %#ok<AGROW>
        MinStruct(count).minarea=Minimos(3).minarea; %#ok<AGROW>
        MinStruct(count).minanomsis=Minimos(3).minanomsis; %#ok<AGROW>
        MinStruct(count).meananomsis=Minimos(3).meananomsis; %#ok<AGROW>
        if(MinStruct(count).additionaldataflag)
          MinStruct(count).minanomsis=Minimos(3).minanomsis; %#ok<AGROW>
          MinStruct(count).maxanomsis=Minimos(3).maxanomsis; %#ok<AGROW>
          MinStruct(count).meananomsis=Minimos(3).meananomsis; %#ok<AGROW>
        end 
    end
    
    


  
    
   
    %======================================================================
    
count=count+1;   
cur_date_num=cur_date_num+config.timefrec/24;


end

