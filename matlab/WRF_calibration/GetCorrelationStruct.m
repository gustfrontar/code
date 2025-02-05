
function [CorrelationStruct]=GetCorrelationStruct( X )

%X contiene N realizaciones (N filas) de un vector aleatorio de S de M
%elementos. El output de esta funcion es un vector con tantos elementos
%como columnas de X que indica cual es la correlacion entre el primer
%elemento de S y los restantes.
% Si asumimos que los elementos de S tienen todos el mismo patron de
% correlacion con sus vecinos podemos recuperar la matriz de correlacion de
% los elementos de S entre si. 

NVar=size(X,2);
NSample=size(X,1);

CorrX=X'*X;

CorrelationStruct=zeros(1,NVar);

for ii = 1:NVar
CorrelationStruct=CorrelationStruct + circshift(squeeze(CorrX(ii,:)),-1);
end

CorrelationStruct=CorrelationStruct/NVar;


end