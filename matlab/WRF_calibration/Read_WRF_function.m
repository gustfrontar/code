function [structure] = Read_WRF_function(startDate, endDate, structure) 

startDateNum=datenum(startDate,'yyyymmdd');
endDateNum=datenum(endDate,'yyyymmdd');

for i=startDateNum:structure.FrequencyForecast:endDateNum
    date=datestr(i,'yyyymmddHH');
    dayDatePath=strcat(structure.Path,date);
    saveDate=datevec(i);
    
    if (isdir(dayDatePath)==1)
        structure.Date((size(structure.Date,1))+1,:)=saveDate;
        % Abre los datos de pronostico del WRF y los promedia segun el
        % plazo de pronostico
        [Data.PH]=ReadAverage_Per_function([structure.Name '.d01.PH'],dayDatePath,structure);    
        [Data.QV]=ReadAverage_Per_function([structure.Name '.d01.QV'],dayDatePath,structure);
        [Data.TH]=ReadAverage_Per_function([structure.Name '.d01.TH'],dayDatePath,structure);
        [Data.TS]=ReadAverage_Per_function([structure.Name '.d01.TS'],dayDatePath,structure);
        [Data.UU]=ReadAverage_Per_function([structure.Name '.d01.UU'],dayDatePath,structure);
        [Data.VV]=ReadAverage_Per_function([structure.Name '.d01.VV'],dayDatePath,structure);
        % Crea la estructura que contiene al pronostico del WRF
        [structure] = Create_WRF_function(structure,Data);
    end   
end
return