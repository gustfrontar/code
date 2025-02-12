clear all
close all


fid=fopen('./LETKF_JMA_BESTTRACK','r');


endfile=false;
ntraj=0;
BestTrack=[];    %Structure where results will be stored.

while( ~endfile )

inline=fgetl(fid);
if(size(inline,1) > 0 & inline ~= -1) 
if( strcmp(inline(1:5),'66666') )  %New trajectory!
ntraj=ntraj+1;
nt=1;
BestTrack(ntraj).name=strtrim(inline(40:60));

elseif(str2num(inline(1:8)) > 0 )
fecha=['20' inline(1:8)];
BestTrack(ntraj).daten(nt)=datenum(fecha,'yyyymmddHH');
BestTrack(ntraj).minlat(nt)=str2num(inline(16:18))/10;
BestTrack(ntraj).minlon(nt)=str2num(inline(20:23))/10;
BestTrack(ntraj).minanomf(nt)=str2num(inline(25:28));
BestTrack(ntraj).maxwind(nt)=str2num(inline(34:36))*0.514444;

if(nt > 1)
lona=BestTrack(ntraj).minlon(nt-1);lata=BestTrack(ntraj).minlat(nt-1);
lonb=BestTrack(ntraj).minlon(nt);latb=BestTrack(ntraj).minlat(nt);
deltat=(BestTrack(ntraj).daten(nt)-BestTrack(ntraj).daten(nt-1))*24;
[ u,v,mag ] = compute_uv_fun( lona,lata,lonb,latb,deltat );
else
u=NaN;v=NaN;mag=NaN;
end
BestTrack(ntraj).uvelf(nt)=u;
BestTrack(ntraj).vvelf(nt)=v;

nt=nt+1;
end
end

if(inline == -1)
endfile=true;
end


end


save('JMA_BESTTRACK.mat','BestTrack');


