clear all
close all



% Abro un archivo donde tengo la latitud y longitud de las estaciones para
% graficarlas en los mapas q siguen a continuacion
datos=load('-ascii', 'estaciones_latlong.txt');
lat_est=datos(:,2);
lon_est=datos(:,3);

largo_est=size(lat_est)


%***************************************************************************
% Me defino una reticula de latitud y longitud para interpolar los datos de
% las estaciones del superensamble
N=26;
lat(1)=-5;
long(1)=-85;


for i=0:N
   for j=0:N
       long(i+1,j+1)=long(1,1)+(2*j);
   end
end
for j=0:N
   for i=0:N
       lat(i+1,j+1)=lat(1,1)-(2*i);
   end
end
%**************************************************************************

mask=zeros(size(lat));


for i=1:N+1
	for j=1:N+1
		aux=sqrt((lat(i,j)-lat_est).^2+(long(i,j)-lon_est).^2);
		min_dist=min(aux);
        	if (min_dist<4.5)
			mask(i,j)=1;
		end		
	end
end


save mascara.mat mask





