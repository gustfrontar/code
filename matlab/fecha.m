% Funcion fecha, genera una variable string con la fecha en el formato
% yyyymmdd
% Los argumentos son la fecha y un corrimiento postivo o negativo.



function valor=fecha(fecha_hoy,offset)

a=datenum(fecha_hoy);
b=a+offset;
c=datevec(b);


d=num2str(c);
valor=strcat(d(1:4),d(9:10),d(15:16));