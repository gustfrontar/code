function [x latloc lonloc] = read_speedy(varnum)

nx=96;
ny=48;
nz=7*4+1; %Variables U, V, T, Q and Surface Pressure
          %U from 1-7, V from 8-14, T from 15-21, Q from 22-28, Surface Pressure 29

path='/Users/alexishannart/Documents/Travail/dada/speedy/CLIMATOLOGY_30YEARS_PERFECT_MODEL_1_196901_360_1/001/';

data_frequency=6;  %Data frequency in hours.

%Date range and total number of dates
ini_date=datenum('1970010100','yyyymmddhh');
end_date=datenum('1998123100','yyyymmddhh');

ntimes=(end_date-ini_date)*24/data_frequency;

%Location of local domains and number of local domains
%[3 37] is Zurich
i_locations=[3];    %i goes from 1 to 96. j=1 at Greenwich
j_locations=[37];   %j=1 at the South Pole and 48 at the North Pole. 
n_locations=length(i_locations);

%Local domain size
local_domain_x=11;
local_domain_y=11;
local_domain_length_x=local_domain_x*2+1;
local_domain_length_y=local_domain_y*2+1;

tmplat=[-87.159 -83.479 -79.777 -76.070 -72.362 -68.652 -64.942 -61.232 -57.521 ...
      -53.810 -50.099 -46.389 -42.678 -38.967 -35.256 -31.545 -27.833 -24.122  ...
      -20.411 -16.700 -12.989  -9.278  -5.567  -1.856   1.856   5.567   9.278  ...
      12.989  16.700  20.411  24.122  27.833  31.545  35.256  38.967  42.678   ...
      46.389  50.099  53.810  57.521  61.232  64.942  68.652  72.362  76.070   ...
      79.777  83.479  87.159];
tmplon=0:3.75:360-3.75;
[speedylon speedylat]=meshgrid(tmplon,tmplat);

%ux = find(and(tmplat<-23,tmplat>-45));
%uy = find(and(tmplon<305,tmplon>285)); 


%Allocate local domain arrays
local_data=NaN([local_domain_length_y local_domain_length_x ntimes n_locations]);


%Get the data
c_date = ini_date;


for itime=1:ntimes
    
    current_year=datestr(c_date,'yyyy');
    
    datestr(c_date,'yyyymmddHH')
    
    filename = [path  datestr(c_date,'yyyymmddHH') '.grd'];

    [state]=read_file(filename , nx , ny , nz);
    
    state = squeeze(state(:,:,varnum));

    for iloc=1:n_locations
      miny=j_locations(iloc)-local_domain_y;
      maxy=j_locations(iloc)+local_domain_y;
      minx=i_locations(iloc)-local_domain_x;
      maxx=i_locations(iloc)+local_domain_x;
      
      if minx<=0 
          local_data(:,:,itime,iloc)=[state(miny:maxy,96+minx:96) state(miny:maxy,1:maxx)];
      else 
          local_data(:,:,itime,iloc) = state(miny:maxy,minx:maxx);
      end

    end
    
    c_date = c_date + data_frequency/24;
end

for iloc=1:n_locations

    miny=j_locations(iloc)-local_domain_y;
    maxy=j_locations(iloc)+local_domain_y;
    minx=i_locations(iloc)-local_domain_x;
    maxx=i_locations(iloc)+local_domain_x;
    
      if minx<=0 
          local_data(:,:,itime,iloc)=[state(miny:maxy,96+minx:96) state(miny:maxy,1:maxx)];

            latloc=[speedylat(miny:maxy,96+minx:96) speedylat(miny:maxy,1:maxx)];
            lonloc=[speedylon(miny:maxy,96+minx:96) speedylon(miny:maxy,1:maxx)];
      else 

        latloc=speedylat(miny:maxy,minx:maxx);
        lonloc=speedylon(miny:maxy,minx:maxx);
      end


    yloc=num2str(j_locations(iloc));
    xloc=num2str(i_locations(iloc));
    
    if j_locations(iloc) < 10 
       yloc= '0' + yloc;
    end
    if i_locations(iloc) < 10 
       xloc= '0' + xloc;
    end
    
    
    %filename = [path '/local_patch_xloc=' xloc '_yloc=' yloc '_localsize=' num2str(local_domain_x) '.mat'];
    x=local_data(:,:,:,:,iloc);
    
    lonloc(lonloc>180)=lonloc(lonloc>180)-360;
    
end

end
