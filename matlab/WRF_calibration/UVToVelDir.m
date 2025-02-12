function [Vel,Dir]=UVToVelDir(U,V)

Vel=sqrt( U.^2 + V.^2 );

Dir=atan2(U,V)*180/pi+180;


end