function [average]=ReadAverage_Per_function(File, Path, struc)

cd(Path)
CAS=importdata(File,' ');
Colheaders = CAS.textdata(1,:);
TextData = CAS.textdata(2:end,:);
Data = CAS.data;

if (File=='PER.d01.TS')
    Data=[Data(:,2),Data(:,6:end)];
    % columna 4 y 5 es u y v
    % Calculo la velocidad del viento (columna 15 de Data)
    Data(:,end+1)=sqrt(Data(:,4).^2+Data(:,5).^2);
    % Calculo la direccion del viento (columna 16 de Data)
    U = Data(:,4)*struc.cosalpha - Data(:,5)*struc.sinalpha;
    V = Data(:,5)*struc.cosalpha + Data(:,4)*struc.sinalpha;
    r2d=45.0/atan(1.0);
    Data(:,end+1)=atan2(U,V)*r2d+180;
end

% Acumulo las variables segun el plazo de pronostico de entrada que esta en
% segundos
struc.Accumulation=struc.Accumulation/3600;j=1;i=1;sup=0;
while sup<struc.LengthForecast
    sup=(struc.Accumulation*(i+2)/2);
    select=(Data(:,1)>(struc.Accumulation*i/2) & Data(:,1)<=(struc.Accumulation*(i+2)/2));
    amount=sum(select);
    average(j,:)=(nansum(Data(select,2:end)))/amount;
    i=i+2;
    j=j+1;
end
cd ..
return