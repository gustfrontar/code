%**************************************************************************
%       ESTA FUNCION CALCULA UN RANK HISTOGRAM.
%**************************************************************************
function [rank_hist] = rank_fun(obs,forecast)
%**************************************************************************
% obs es un vector columna con las observaciones.
% forecast es una matriz. Cada columna es un pronóstico distinto (un
% miembro del ensemble).

a=size(forecast);
ens=a(2); 
ndatos=length(obs);

rank_hist=zeros(1,ens+1);


%Para 24 horas (sobre el total de los datos)
for idato=1:ndatos
  %Primero me fijo que no existan pronósticos iguales a la verificación,
  %esto es sobre todo importante para la lluvia donde el pronóstico 0 puede
  %dar lugar a muchos miembros con el mismo valor que la verificacion en
  %muchos puntos.
  if(isnan(obs(idato))==0 & isnan(forecast(idato,1))==0) %Solo aportan aquellos donde el dato no es NaN.
  i_eq=find(forecast(idato,:) == obs(idato));
  %Si existen pronosticos iguales a la verificacion entonces los perturbo.
  if(length(i_eq) > 0 )
      for j=1:length(i_eq)
          forecast(idato,i_eq(j))=forecast(idato,i_eq(j))+(rand-0.5)/100;
      end
  end
  clear i_eq
  i_rank=find(forecast(idato,:) < obs(idato));
  
  rank_hist(length(i_rank)+1)=rank_hist(length(i_rank)+1)+1;  

  clear i_rank  
  end
end

%**************************************************************************






