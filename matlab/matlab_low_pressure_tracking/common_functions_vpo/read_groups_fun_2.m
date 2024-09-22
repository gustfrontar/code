function [group]=read_groups_fun_2(config)



end_date_num=datenum(config.date_end,'yyyymmddHH');
cur_date_num=datenum(config.date_ini,'yyyymmddHH');

%Initialize variables
minlat=[];
minlon=[];
meanlat=[];
meanlon=[];
minlatanalysis=[];
minlonanalysis=[];
minanom=[];
minlap=[];
minanomanalysis=[];
minlapanalysis=[];

trajidanalysis=[];

date=[];


while ( cur_date_num <= end_date_num )

clear group
load_file=[config.grouppath 'GROUP_' datestr(cur_date_num,'yyyymmddHH') '_L' num2str(config.forecastlength) '.mat'];
load(load_file);

minlat=cat(3,minlat,group.minlat);
minlon=cat(3,minlon,group.minlon);
minanom=cat(3,minanom,group.minanom);
minlap=cat(3,minlap,group.minlap);

meanlat=cat(2,meanlat,group.meanlat);
meanlon=cat(2,meanlon,group.meanlon);

minlatanalysis=cat(2,minlatanalysis,group.minlatanalysis);
minlonanalysis=cat(2,minlonanalysis,group.minlonanalysis);
minanomanalysis=cat(2,minanomanalysis,group.minanomanalysis);

minlapanalysis=cat(2,minlapanalysis,group.minlapanalysis);

trajidanalysis=cat(2,trajidanalysis,group.trajidanalysis);

date=[date ones(1,group.ngroup)*cur_date_num];

cur_date_num=cur_date_num+config.resultfrec/24;

end

group.minlat=minlat;
group.minlon=minlon;
group.meanlat=meanlat;
group.meanlon=meanlon;
group.minlatanalysis=minlatanalysis;
group.minlonanalysis=minlonanalysis;
group.minanom=minanom;
group.minanomanalysis=minanomanalysis;
group.minlapanalysis=minlapanalysis;
group.minlap=minlap;
group.date=date;
group.trajidanalysis=trajidanalysis;




