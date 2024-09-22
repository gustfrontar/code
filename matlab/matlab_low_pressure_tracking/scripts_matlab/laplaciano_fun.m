%Calcula el laplaciano en coordenadas esfericas
%Por el momento solo lo hace como si fueran rectangulares.

function [laplaciano]=laplaciano_fun(data,lat,lon);

[nfilas ncols ntiempos]=size(data);

for i=1:ntiempos
    [auxx auxy]=gradient(data(:,:,i));
    laplaciano(:,:,i)=divergence(auxx,auxy);
end