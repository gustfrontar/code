%Este script define variables que son comunes a todos los archivos que vamos a manipular para la verificacion
%del pronostico probabilistico de lluvia en una resolucion de 1ºx1º.



nvar_ensemble=9; %numero de variables en el archivo del ensemble.
nvar_verif=1;   %numero de variables en el archivo del cmorph.
nxmax=51
nymax=46
record_size=[nxmax nymax];
resolucion=1;                %Resolucion horizontal en grados.

%Defino las latitudes.
for j=1:nymax
    lat(j)=j-46;
end
%Defino las longitudes.
for i=1:nxmax
lon(i)=i-91;
end

%Defino el nombre de los archivos con el control.

arch=char('20051022','20051023','20051024','20051025','20051026','20051027','20051028','20051029','20051030','20051031','20051101','20051102','20051103','20051104','20051105','20051106','20051107','20051108','20051109','20051110','20051111','20051112','20051113','20051114','20051115','20051116','20051117','20051118','20051119','20051120','20051121','20051122','20051123','20051124','20051125','20051126','20051127','20051128','20051129');
