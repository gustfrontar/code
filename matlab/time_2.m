
function tempo=time_arch(fecha_hoy, offset)

a=datenum(fecha_hoy);
b=a+offset;
c=datevec(b);
tempo=datestr(c, 'yyyymmdd');

