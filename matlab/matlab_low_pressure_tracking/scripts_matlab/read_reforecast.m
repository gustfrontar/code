clear all
close all

add_offset=32066;
ncload ../DATA/Reanalisis/X190.183.78.55.65.14.10.33.nc

hgttotal=[];
hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.10.55.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.11.29.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.11.51.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.12.27.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.12.54.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.13.31.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.13.56.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.13.9.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.14.17.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.14.54.nc

hgttotal=[hgttotal ; hgt];

ncload ../DATA/Reanalisis/X190.183.78.55.65.14.15.12.nc

hgttotal=[hgttotal ; hgt];

hgttotal=hgttotal+add_offset;
%Vamos a calcular la media y la desviacion estandard.

mean_hgt=squeeze(nanmean(hgttotal,1));
std_hgt=squeeze(nanstd(hgttotal,1));

%Tomamos la variable hgt del ultimo archivo.
hgt=squeeze(hgt+add_offset);

for i=1:size(hgt,1)
   anomalia(:,:,i)=(squeeze(hgt(i,:,:))-mean_hgt); %./std_hgt;
   anomalia_filt(:,:,i)=fast_filter_fun(anomalia(:,:,i),3);
end




tiempos=1:size(hgt,1);
tiempos=tiempos/4;
save('../DATA/Reanalisis/anomalia.mat','anomalia','anomalia_filt','tiempos','hgt')
