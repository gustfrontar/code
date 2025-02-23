clear all
close all

%**********
datos=load('-ascii', 'est_cuenca.txt');
lat_est=datos(:,2);
lon_est=datos(:,3);
%*********



% Defino la reticula q quiero interpolar. Es la reticula q usa el grads aprox    
nx=110;
ny=85;
for j=0:109
    for i=0:84
        lat_grad(i+1,j+1)=22-i;
    end
end
for i=0:84
    for j=0:109
        lon_grad(i+1,j+1)=-114+j;
    end
end

% Renombro las variables para poder graficar
lo=lon_grad;
la=lat_grad;

% Abro el archivo con la mascara de la cuenca
narch=fopen('mask_cuenca.dat','r','l'); 
cuenca=fread(narch,[nx ny],'single');
cuenca=cuenca';
mask1=cuenca;
mask2=cuenca;
mask3=cuenca;
mask4=cuenca;
maskt=cuenca;

% Separo las diferentes mascaras para interpolar cada una por separado
valor1=find(mask1~=1);
mask1(valor1)=0;
mask1=flipdim(mask1,1);

valor2=find(mask2~=2);
mask2(valor2)=0;
mask2=flipdim(mask2,1);

valor3=find(mask3~=3);
mask3(valor3)=0;
mask3=flipdim(mask3,1);

valor4=find(mask4~=4);
mask4(valor4)=0;
mask4=flipdim(mask4,1);

maskt=flipdim(maskt,1);



% Abro las latitudes y longitudes del WRF
load lat_lon_wrf.mat

%Defino la reticula a la cual quiero interpolar q es la reticula del WRF 
for j=0:100
    for i=0:109
        lati(i+1,j+1)=lat(i+1);
        %lati(i+1,j+1)=lat(110-i);
    end
end
for i=0:109
    for j=0:100
        long(i+1,j+1)=lon(j+1);
    end
end

% Me genero un vector con los datos q quiero interpolar
lon_grad=reshape(lon_grad,nx*ny,1);
lat_grad=reshape(lat_grad,nx*ny,1);
vector1=reshape(mask1,nx*ny,1);
vector2=reshape(mask2,nx*ny,1);
vector3=reshape(mask3,nx*ny,1);
vector4=reshape(mask4,nx*ny,1);
vectort=reshape(maskt,nx*ny,1);

% Interpolo con la funcion griddata
mascara1=griddata(lon_grad,lat_grad,vector1,long,lati);
mascara2=griddata(lon_grad,lat_grad,vector2,long,lati);
mascara3=griddata(lon_grad,lat_grad,vector3,long,lati);
mascara4=griddata(lon_grad,lat_grad,vector4,long,lati);
mascarat=griddata(lon_grad,lat_grad,vectort,long,lati);






% Transformo todas la mascaras en valores de 1 y ceros donde no es la
% cuenca
val1=find(mascara1~=0);
mascara1(val1)=1;
mascara1=mascara1;

val2=find(mascara2~=0);
mascara2(val2)=1;
mascara2=mascara2;

val3=find(mascara3~=0);
mascara3(val3)=1;
mascara3=mascara3;

val4=find(mascara4~=0);
mascara4(val4)=1;
mascara4=mascara4;

valt=find(mascarat~=0);
mascarat(valt)=1;
mascarat=mascarat;

mascaras(:,:,1)=mascara1;
mascaras(:,:,2)=mascara2;
mascaras(:,:,3)=mascara3;
mascaras(:,:,4)=mascara4;

[maxval maxloc]=max(mascaras,[],3);
maxloc(maxval==0)=0;

mascara1=(maxloc==1);
mascara2=(maxloc==2);
mascara3=(maxloc==3);
mascara4=(maxloc==4);

suma=mascara1*1+mascara2*2+mascara3*3+mascara4*4;

% Guardo las mascaras en un .mat. Cada una por separado y toda en total
%save mask_interpolada.mat mascara1 mascara2 mascara3 mascara4 mascarat



% Renombro las lat y lon para poder graficar
lonvar=long;
latvar=lati;
carga_mapa
v=[0 1 2 3 4 5 6 7 8 9 10];
% %vcol=[2 31 41 43 45 47 55 57 59 27 ];
vcol=[2 55 53 45 42 32 21 24 27 29];
load coast


% Graficos 

figure
pcolor(lonvar,latvar,suma)
shading flat
hold on
% pcolor(lonvar,latvar,mascara1)
plot(lon_est,lat_est,'LineStyle','none','Marker','o','MarkerSize',6,'MarkerEdgeColor',[0 0 1],'MarkerFaceColor',[1 0 0.2])

plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
%plot(lon_ciudad,lat_ciudad,'ro')
xlabel('Longitud','FontSize',12);
ylabel('Latitud','FontSize',12);
[colores] = plot_jrcol(v,vcol,1);
axis([-100 -30 -61 -3])
print('-dpng','mascara')

figure
pcolor(lo,la,maskt)
shading flat
hold on
plot(lon_est,lat_est,'LineStyle','none','Marker','o','MarkerSize',6,'MarkerEdgeColor',[0 0 1],'MarkerFaceColor',[1 0 0.2])

plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
xlabel('Longitud','FontSize',12);
ylabel('Latitud','FontSize',12);
[colores] = plot_jrcol(v,vcol,1);
print('-dpng','mascara_2')






