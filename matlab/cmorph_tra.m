

function [transecta]=cmorph_tra(XX,YY,XXs,XXe,YYs,YYe,pp)

%##########################################################################
% Calcula la transecta del cmorph y hace un hovmoller
%##########################################################################

[nx]=size(XX);
[ny]=size(YY);

%Calculamos el numero de puntos optimos para la transecta buscando respetar
%la resolucion horizontal original.
npuntos=4*round(( (XXe-XXs)^2 + (YYe-YYs)^2)^0.5);
Xinc=(XXe-XXs)/npuntos;
Yinc=(YYe-YYs)/npuntos;
Xt=XXs:Xinc:XXe;
Yt=YYs:Yinc:YYe;
if(Xinc==0)
    Xt=XXs*ones(size(Yt));
elseif (Yinc==0)
    Yt=YYs*ones(size(Xt));
end

transecta=interp2(XX,YY,pp,Xt,Yt);

