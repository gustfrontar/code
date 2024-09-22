function MinStruct=get_minimun_struct(config)

%This function prepares the basic minimun information needed for the
%tracking routine.

ini_date_num=datenum(config.date_ini,'yyyymmddHH');
end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;
%This function will chek for premature splitings.

while ( cur_date_num <= end_date_num )

    %READ DATA...
    if(config.isforecast)
    MinFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '_F' num2str(int32((count-1)*config.timefrec)) '_M' num2str(int32(config.ensemblemember)) '.bin'];   
    else    
    MinFileName=[config.result_path '/MINIMOS/' datestr(cur_date_num,config.datadateformat) '.bin'];
    end
    
    tmpstruct=read_min_fun_2(MinFileName);

    %Save some fields to the MinStruct
        MinStruct(count).daten=cur_date_num; %#ok<AGROW>
        MinStruct(count).nf=tmpstruct.nf; %#ok<AGROW>
        MinStruct(count).nminimos=tmpstruct.nminimos; %#ok<AGROW>
        MinStruct(count).minlat=tmpstruct.minlatint; %#ok<AGROW>
        MinStruct(count).minlon=tmpstruct.minlonint; %#ok<AGROW>
        MinStruct(count).additionaldataflag=tmpstruct.additionaldataflag; %#ok<AGROW>
        MinStruct(count).minlap=tmpstruct.minlap;
        MinStruct(count).minarea=tmpstruct.minarea; %#ok<AGROW>
        MinStruct(count).minanomsis=tmpstruct.minanomsis; %#ok<AGROW>
        MinStruct(count).meananomsis=tmpstruct.meananomsis; %#ok<AGROW>

        if(MinStruct(count).additionaldataflag)
          MinStruct(count).minanomsis=tmpstruct.minanomsis; %#ok<AGROW>
          MinStruct(count).maxanomsis=tmpstruct.maxanomsis; %#ok<AGROW>
          MinStruct(count).meananomsis=tmpstruct.meananomsis; %#ok<AGROW>
        end 

%======================================================================
    
count=count+1;   
cur_date_num=cur_date_num+config.timefrec/24;

end

