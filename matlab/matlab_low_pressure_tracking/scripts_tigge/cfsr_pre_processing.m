clear all
close all
%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');
%PRE PROCESAMIENTO PARA LOS DATOS DEL CFSR.

datapath=['../DATA/CFSR/HGTVPO/'];
outpath=datapath;
mkdir(outpath)

RES_TIGGE=2.0;

LATTIGGE=[-90:RES_TIGGE:90];
LONTIGGE=[0:RES_TIGGE:360-RES_TIGGE];
[LONTIGGE2D LATTIGGE2D]=meshgrid(LONTIGGE,LATTIGGE);

%LISTAMOS EL DIRECTORIO Y OBTENEMOS EL NUMERO DE ARCHIVOS.

a=dir(datapath);

nfiles=size(a);

%PARA CADA ARCHIVO VAMOS A DECODIFICAR E INTERPOLAR EL CONTENIDO.

[nyt nxt]=size(LATTIGGE);

first_file=true;
for ifile=1:nfiles %nfiles %las primeras 2 son el . y el ..
    
   
    
    %Verifico que el archivo comience con pgbhnl usando expresiones
    %regulares.
    if (~isempty(regexpi(a(ifile).name,'^pgbhnl')))
        
        
        
        
        
    
      CFSR_FILE=[datapath '/' a(ifile).name]; 
        
      tmpfile='tmpfile.bin';
      [data inventory] = read_grib(CFSR_FILE,2,'500','');
      
      
      %for ii=1:size(data,3);
      %USO UN FILTRO DE ARMONICOS ESFERICOS PARA REGRILLAR LOS DATOS Y
      %LLEVARLOS A LA RETICULA DEL TIGGE.
      %[filtered_grid]=spherical_harmonic_filter(data(:,:,ii),inventory.grid_lat,inventory.grid_lon,62,LATTIGGE,LONTIGGE);
      
      %USO PROMEIDO POR CAJAS PARA LLEVAR EL CFSR A LA RETICULA DE BAJA
      %RESOLUCION.
   [LON LAT]=meshgrid(inventory.grid_lon,inventory.grid_lat);
   [nyt nxt]=size(LONTIGGE2D);
   if(first_file) 
   for iy=1:nyt
    for ix=1:nxt
        
         %Defino los bordes de la caja considerando todas las
         %posibilidades.

         y_s=LATTIGGE2D(iy,ix)-(RES_TIGGE)/2;
         y_n=LATTIGGE2D(iy,ix)+(RES_TIGGE)/2;

         x_w=LONTIGGE2D(iy,ix)-(RES_TIGGE)/2;
         x_e=LONTIGGE2D(iy,ix)+(RES_TIGGE)/2;
         
        
         %Aplico promedio por cajas con condicion de borde ciclica en X no
         %tiene condicion de borde polar.
         %*****************************************************************
         indexinterp{iy,ix}=find(  LAT < y_n & LAT >= y_s & (LON < x_e | LON > x_e + 360 ) & (LON >= x_w | LON <= x_w - 360 ));
         %*****************************************************************

    end
   end
   first_file=false;
   end
      
      
    for it=1:size(data,3)
      filtered_grid=NaN(nyt,nxt);
      tmp1=data(:,:,it);
      for iy=1:nyt
        for ix=1:nxt
         %tmp2=tmp1(indexinterp);
         filtered_grid(iy,ix)=mean(tmp1(indexinterp{iy,ix}));
         %end
        end 
      end
       
      %Fuerzo a que todos los puntos tengan el mismo valor en el polo.
      filtered_grid(1,:)=squeeze(mean(filtered_grid(1,:)));
      filtered_grid(end,:)=squeeze(mean(filtered_grid(end,:)));
      
      
      datenumber=datenum(inventory.year(it),inventory.month(it),inventory.day(it),inventory.hour(it),0,0);
      filedate=datestr(datenumber,'yyyymmddHH');
      outfile=[outpath '/CFSR_HGT_' filedate '.bin'];
      
      fid=fopen(outfile,'w');
      fwrite(fid,filtered_grid,'single');
      fclose(fid);
      
      end

    end
end


      fileout=[outpath '/CFSR_LON.bin'];
      fid=fopen(fileout,'w');
      fwrite(fid,LONTIGGE2D,'single');
      fclose(fid);

      fileout=[outpath '/CFSR_LAT.bin'];
      fid=fopen(fileout,'w');
      fwrite(fid,LATTIGGE2D,'single');
      fclose(fid);


















