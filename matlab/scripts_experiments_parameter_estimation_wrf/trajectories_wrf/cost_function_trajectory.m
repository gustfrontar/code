%ESTA VERSION DE LA FUNCION SIRVE PARA HACER PRUEBAS Y CORRECCIONES A LAS
%FUNCIONES DE COSTO.

function [cost_function]=test_cost_function_trajectory(lona,lata,lonb,latb,lonc,latc,deltat1,deltat2,Vmax,type)
w1=0.5;
w2=0.5;
%TYPE CONTROLA EL TIPO DE FUNCION DE COSTO.
% TYPE=1 HODGES ET AL, TIENE EN CUENTA LA CONTINUIDAD EN LA DIRECCION Y EN
% LA MAGNITUD DE LAS VELOCIDADES
% TYPE=2 CALCULA EL MODULO DE LA DIFERENCIA DEL VECTOR VELOCIDAD.

deltax1=diff_lon_fun(lonb,lona)*cosd((latb+lata)/2)*111000;
deltay1=(latb-lata)*111000;
deltax2=diff_lon_fun(lonc,lonb)*cosd((latc+latb)/2)*111000;
deltay2=(latc-latb)*111000;


if(type==1)
%CALCULO EL TERMINO QUE TIENE QUE VER CON LA CONTINUIDAD EN LA TRAYECTORIA.
if( (deltax1^2+deltay1^2)==0 || (deltax2^2+deltay2^2)==0 )
primer_termino=0.5;
else
primer_termino=0.5*(1-(deltax1*deltax2 + deltay1*deltay2)/( sqrt(deltax1^2+deltay1^2) * sqrt(deltax2^2+deltay2^2) ));
end

if( (deltax1^2+deltay1^2)==0 && (deltax2^2+deltay2^2)==0 )
primer_termino=0;
end

%CALCULO EL TERMINO QUE TIENE QUE VER CON LA CONTINUIDAD EN LA VELOCIDAD.
vel1=sqrt( deltax1^2 + deltay1^2)/deltat1;
vel2=sqrt( deltax2^2 + deltay2^2)/deltat2;

if(vel1==0 && vel2==0)
    segundo_termino=0;
else
    segundo_termino=( 1 -  2* sqrt(vel1*vel2)/( vel1 + vel2));
end
%cost_function=w1*primer_termino+w2*segundo_termino;
maxvel=max([vel1 vel2]);
cost_function=w1*primer_termino +w2*segundo_termino;

end

if(type==2)

cost_function_ref=Vmax/deltat1;
%En este caso asumimos que deltat1==deltat2;
cost_function= (1/cost_function_ref)*sqrt((deltax1/deltat1-deltax2/deltat2)^2+(deltay1/deltat1-deltay2/deltat2)^2)/deltat1;
%por ejemplo si el cambio de velocidad es igual a Vmax

end

if(type==3)

cost_function_ref=Vmax/deltat1;
%En este caso asumimos que deltat1==deltat2;
cost_function= (1/cost_function_ref)*sqrt((deltax1/deltat1-deltax2/deltat2)^2+(deltay1/deltat1-deltay2/deltat2)^2)/deltat1;

vel1=sqrt( deltax1^2 + deltay1^2)/deltat1;
vel2=sqrt( deltax2^2 + deltay2^2)/deltat2;
maxvel=max([vel1 vel2]);

cost_function=cost_function*((1+Vmax)/(1+maxvel))^(1/2);

%por ejemplo si el cambio de velocidad es igual a Vmax

end

