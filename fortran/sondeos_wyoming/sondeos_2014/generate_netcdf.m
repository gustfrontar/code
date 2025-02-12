clear all
close all

nx_w=149;
ny_w=299;
nz_w=20;
nt_w=49;
nzsoil_w=4;
time_skip=3;

init_date='2013_06_20_00_00_00';
tiempos=[0:time_skip:nt_w];
filename=['../data/outputd01.dat'];
filedomain=['/home/wrf/arch/datos/dominio/dominio.dat'];
filename_nc=['../data/output_wrf_' init_date '.nc']

%Generate netcdf file.
%============ CREATE FILE AND GLOBAL ATT ==================
ncid = netcdf.create(filename_nc,'NC_WRITE');

globalid=netcdf.getConstant('NC_GLOBAL')
%Define mode

%Define dims
xdimid = netcdf.defDim(ncid,'X',nx_w);
ydimid = netcdf.defDim(ncid,'Y',ny_w);
tdimid = netcdf.defDim(ncid,'TIME',length(tiempos));
vectorid= netcdf.defDim(ncid,'VECTORDIM',2);

%Create global attributes
netcdf.putAtt(ncid,globalid,'TYPE','MODEL_OUTPUT');
netcdf.putAtt(ncid,globalid,'NAME','WRF');
netcdf.putAtt(ncid,globalid,'MODEL_TYPE','REGIONAL');
netcdf.putAtt(ncid,globalid,'INITDATE',init_date);
netcdf.putAtt(ncid,globalid,'DATATYPE','FIELD');
netcdf.putAtt(ncid,globalid,'RESOLUTION',15000);
netcdf.putAtt(ncid,globalid,'LAT_VAR','LAT');
netcdf.putAtt(ncid,globalid,'LON_VAR','LON');
%============ DEFINE VARIABLES AND ATTS ==================

%LON
varid   = netcdf.defVar(ncid,'LON','double',[ydimid xdimid]);
netcdf.putAtt(ncid,varid,'PLOT','false');
netcdf.putAtt(ncid,varid,'NAME','LONGITUD');
netcdf.putAtt(ncid,varid,'UNITS','DEG');
varidlon=varid;
%LAT
varid   = netcdf.defVar(ncid,'LAT','double',[ydimid xdimid]);
netcdf.putAtt(ncid,varid,'PLOT','false');
netcdf.putAtt(ncid,varid,'NAME','LATITUD');
netcdf.putAtt(ncid,varid,'UNITS','DEG');
varidlat=varid;

%TEMPERATURA 2M 
varid   = netcdf.defVar(ncid,'T2','double',[ydimid xdimid tdimid]);
netcdf.putAtt(ncid,varid,'PLOT','true');
netcdf.putAtt(ncid,varid,'NAME','Temperatura a 2 metros');
netcdf.putAtt(ncid,varid,'UNITS','C');
netcdf.putAtt(ncid,varid,'GROUP','Temperatura');
netcdf.putAtt(ncid,varid,'SCALE','default');
netcdf.putAtt(ncid,varid,'CONTOUR_COLOR','default');
netcdf.putAtt(ncid,varid,'PLOT_INTERVALS',[-10:2:44]);
netcdf.putAtt(ncid,varid,'LEAD_TIME',tiempos);
netcdf.putAtt(ncid,varid,'PLOT_TYPE','shaded,contour');
netcdf.putAtt(ncid,varid,'FIELD_TYPE','field');
netcdf.putAtt(ncid,varid,'LEVEL','Surface');
netcdf.putAtt(ncid,varid,'USER','all');
varidT2=varid;
%TEMPERATURA 850 hPa 
varid   = netcdf.defVar(ncid,'T850HPA','double',[ydimid xdimid tdimid]);
netcdf.putAtt(ncid,varid,'PLOT','true');
netcdf.putAtt(ncid,varid,'NAME','Temperatura en 850 hPa.');
netcdf.putAtt(ncid,varid,'UNITS','C');
netcdf.putAtt(ncid,varid,'GROUP','Temperatura');
netcdf.putAtt(ncid,varid,'SCALE','default');
netcdf.putAtt(ncid,varid,'CONTOUR_COLOR','default');
netcdf.putAtt(ncid,varid,'PLOT_INTERVALS',[-20:2:30]);
netcdf.putAtt(ncid,varid,'LEAD_TIME',tiempos);
netcdf.putAtt(ncid,varid,'PLOT_TYPE','shaded,contour');
netcdf.putAtt(ncid,varid,'FIELD_TYPE','field');
netcdf.putAtt(ncid,varid,'LEVEL','850 hPa.');
netcdf.putAtt(ncid,varid,'USER','advanced');
varidT850HPA=varid;

%VIENTO  (INTENSIDAD)
varid   = netcdf.defVar(ncid,'VIENTO10','double',[ydimid xdimid tdimid vectorid]);
netcdf.putAtt(ncid,varid,'PLOT','true');
netcdf.putAtt(ncid,varid,'NAME','Viento a 10 metros');
netcdf.putAtt(ncid,varid,'UNITS','m/s');
netcdf.putAtt(ncid,varid,'GROUP','Viento');
netcdf.putAtt(ncid,varid,'SCALE','default');
netcdf.putAtt(ncid,varid,'CONTOUR_COLOR','default');
netcdf.putAtt(ncid,varid,'PLOT_INTERVALS',[0:2:30]);
netcdf.putAtt(ncid,varid,'LEAD_TIME',tiempos);
netcdf.putAtt(ncid,varid,'PLOT_TYPE','vector,shaded,contour');
netcdf.putAtt(ncid,varid,'FIELD_TYPE','vector');
netcdf.putAtt(ncid,varid,'LEVEL','Surface');
netcdf.putAtt(ncid,varid,'USER','all');
varidVIENTO10=varid;

%VIENTO A 850 hPa.
varid   = netcdf.defVar(ncid,'VIENTO850HPA','double',[ydimid xdimid tdimid vectorid]);
netcdf.putAtt(ncid,varid,'PLOT','true');
netcdf.putAtt(ncid,varid,'NAME','Viento en 850 hPa.');
netcdf.putAtt(ncid,varid,'UNITS','m/s');
netcdf.putAtt(ncid,varid,'GROUP','Viento');
netcdf.putAtt(ncid,varid,'SCALE','default');
netcdf.putAtt(ncid,varid,'CONTOUR_COLOR','default');
netcdf.putAtt(ncid,varid,'PLOT_INTERVALS',[0:2:30]);
netcdf.putAtt(ncid,varid,'LEAD_TIME',tiempos);
netcdf.putAtt(ncid,varid,'PLOT_TYPE','vector,shaded,contour');
netcdf.putAtt(ncid,varid,'FIELD_TYPE','vector');
netcdf.putAtt(ncid,varid,'LEVEL','850 hPa.');
netcdf.putAtt(ncid,varid,'USER','all');
varidVIENTO850HPA=varid;

%MAX_DBZ (CON ESCALA PROPIA)
varid   = netcdf.defVar(ncid,'MAXDBZ','double',[ydimid xdimid tdimid]);
netcdf.putAtt(ncid,varid,'PLOT','true');
netcdf.putAtt(ncid,varid,'NAME','Reflectividad Maxima');
netcdf.putAtt(ncid,varid,'UNITS','C');
netcdf.putAtt(ncid,varid,'GROUP','Conveccion');
netcdf.putAtt(ncid,varid,'SCALE','own');
netcdf.putAtt(ncid,varid,'SCALE_R',[200 175 130 95 75 60 40 30 20]);
netcdf.putAtt(ncid,varid,'SCALE_G',[255 240 210 190 180 170 150 140 130]);
netcdf.putAtt(ncid,varid,'SCALE_B',[255 255 255 250 240 230 210 200 190]);
netcdf.putAtt(ncid,varid,'CONTOUR_COLOR','default');
netcdf.putAtt(ncid,varid,'PLOT_INTERVALS',[0 10 20 30 40 50 60 70]);
netcdf.putAtt(ncid,varid,'LEAD_TIME',tiempos);
netcdf.putAtt(ncid,varid,'PLOT_TYPE','shaded,contour');
netcdf.putAtt(ncid,varid,'FIELD_TYPE','field');
netcdf.putAtt(ncid,varid,'LEVEL','Surface');
netcdf.putAtt(ncid,varid,'USER','all');
varidMAXDBZ=varid;


netcdf.endDef(ncid)
%============ DEFINE VARIABLES AND ATTS ==================



%Read domain
nfile=fopen(filedomain,'r','b');

LANDMASK=fread(nfile,[nx_w ny_w],'single')';
HGT=fread(nfile,[nx_w ny_w],'single')';
LAT=fread(nfile,[nx_w ny_w],'single')';
LON=fread(nfile,[nx_w ny_w],'single')';

%SAVE VARIABLES TO NETCDF:
netcdf.putVar(ncid,varidlon,LON);
netcdf.putVar(ncid,varidlat,LAT);

fclose(nfile)

%Read data
%=============================================================
nfile=fopen(filename,'r','b');

for it=1:nt_w

for ii=1:nz_w
 W=fread(nfile,[nx_w ny_w],'single')';
end
 Q2=fread(nfile,[nx_w ny_w],'single')';
 T2=fread(nfile,[nx_w ny_w],'single')';
for ii=1:nz_w
 QVAPOR=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 QRAIN=fread(nfile,[nx_w ny_w],'single')';
end
for i=1:nz_w
 QICE=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 QSNOW=fread(nfile,[nx_w ny_w],'single')';
end
for i=1:nz_w
 QGRAUP=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nzsoil_w
 TSLB=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nzsoil_w
 SMOIS=fread(nfile,[nx_w ny_w],'single')';
end
 SST=fread(nfile,[nx_w ny_w],'single')';
 fread(nfile,[nx_w ny_w],'single')';
 TSK=fread(nfile,[nx_w ny_w],'single')';
 RAINC=fread(nfile,[nx_w ny_w],'single')';
 RAINNC=fread(nfile,[nx_w ny_w],'single')';
 SNOWNC=fread(nfile,[nx_w ny_w],'single')';
 PBLH=fread(nfile,[nx_w ny_w],'single')';
for ii=1:nz_w
 HEIGHT=1000*fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 TC=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 THETA=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 TD=fread(nfile,[nx_w ny_w],'single')';
end
 TD2=fread(nfile,[nx_w ny_w],'single')';
for ii=1:nz_w
 fread(nfile,[nx_w ny_w],'single')';
end
 CLFLO=fread(nfile,[nx_w ny_w],'single')';
 CLFMI=fread(nfile,[nx_w ny_w],'single')';
 CLFHI=fread(nfile,[nx_w ny_w],'single')';
for ii=1:nz_w
 UMET=fread(nfile,[nx_w ny_w],'single')';
end
for ii=1:nz_w
 VMET=fread(nfile,[nx_w ny_w],'single')';
end
 U10M=fread(nfile,[nx_w ny_w],'single')';
 V10M=fread(nfile,[nx_w ny_w],'single')';
 SLP=fread(nfile,[nx_w ny_w],'single')';
 MAX_DBZ=fread(nfile,[nx_w ny_w],'single')';
 MCAPE=fread(nfile,[nx_w ny_w],'single')';
end %End over times

%SAVE DATA TO NETCDF ============================================
if( mod(it,time_skip) == 0)  %Solo escribe si corresponde para ese tiempo.

init_pos=it/time_skip;
netcdf.putVar(ncid,varidT2,[init_pos 1 1],[1 ny_w nx_w],T2);
netcdf.putVar(ncid,varidT850HPA,[init_pos 1 1],[1 ny_w nx_w],squeeze(TC(:,:,6)));
TMP(1,:,:)=U10M;
TMP(2,:,:)=V10M;
netcdf.putVar(ncid,varidVIENTO10,[1 init_pos 1 1],[2 1 ny_w nx_w],TMP);
TMP(1,:,:)=squeeze(UMET(:,:,6));
TMP(2,:,:)=squeeze(VMET(:,:,6));
netcdf.putVar(ncid,varidVIENTO850HPA,[1 init_pos 1 1],[2 1 ny_w nx_w],TMP);
netcdf.putVar(ncid,varidMAXDBZ,[init_pos 1 1],[1 ny_w nx_w],MAXDBZ);

end

fclose(nfile)
%=============================================================




netcdf.close(ncid)

