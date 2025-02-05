%**************************************************************************
%       ESTA FUNCION CALCULA LA PROBABILIDAD EN FUNCION DE LA PRECIPITACION
%       PARA UN PRONOSTICO DETERMINISTICO.
%       Script original Febrero 2007 modificado en Enero de 2008 con
%       inclusion de mas opciones y vectorizacion parcial.
%       Modificada en enero de 2009 para agregar el computo de la
%       probabilidad de los 0.
%**************************************************************************

function [f_out n_out categorias_out] = pre_cal_fun_1bnoreg(obs_in,forecast_in,umbral,categorias,sm_flag,dyn_cal_flag,ndyncat)
%**************************************************************************
%sm_flag si la funcion de probabilidad obtenida va a ser suavizada o no
%si es 1 aplico un suavizado, sino no.
min_datos=0; %Numero minimo de datos para que podamos computar la 
%probabilidad en una categoria.
%Un buen valor para min_datos es 0, nos permite jugar mas adelante con ese
%valor.
%La variable categorias controla los intervalos entre para los cuales vamos
%a calcular la probabilidad condicional de ocurrencia de los distintos
%valores de precipitacion.
nsmooth=2; %Cuantas veces repetimos el ciclo de suavizado cuando la flag sm_flag esta activada.

%La estrucutra de obs_in debe ser la siguiente.
%Cada columna es un conjunto de observacion diferente (por ejemplo
%pluviometros y CMORPH). Cada fila representa un punto, un valor de
%precipitacion que corresponde a una fecha y lugar determinado. Esta fecha
%y lugar determinado debe coincidir con las filas de la matriz forecast_in.
%La estructura de forecast_in debe ser la siguiente.
%La primera dimension es la posici???n y fecha que coincide con la de obs.
%La segunda dimension son los conjuntos o ensambles diferentes,
%tambien puede ser la prevision del ensemble a distintas horas (lo mas
%comun).

%19 septiembre 2008
%DYN_CAL_FLAG = si es true entonces las categorias se determinan en forma
%dynamica para que todas las categorias (excepto el 0) tengan la misma
%cantidad de datos.
%NDYNCAT determina el numero de categorias que se calculan usando este
%algoritmo.

forecast=forecast_in;
obs=obs_in;

ndatos=length(forecast);
ncat=length(categorias);
numb=length(umbral);

nanes=sum(isnan(forecast)==0);

if(dyn_cal_flag)
    ncat=ndyncat+1;
end

if(nanes > 0)

     
%**************************************************************************
%   LO PRIMERO QUE HAGO ES DETERMINAR LAS CATEGORIAS SI ESTA PRENDIDA LA
%   OPCION DE CATEGORIAS DINAMICAS, SINO USO LAS QUE INGRESAN A LA FUNCION
%   A TRAVES DE LA VARIABLE CATEGORIAS. 
%**************************************************************************

if(dyn_cal_flag)
    clear categorias
    aux=forecast(forecast > 0 & obs >= 0); %Me quedo con el subconjunto que
                                           %donde el pronostico es mayor
                                           %que 0 y hay una observacion
                                           %disponible.
    aux=sort(aux,'ascend');
    naux=length(aux);
    
    %Definimos las categorias en base a los ndyncat tiles de la
    %distribucion para que cada categoria tenga el mismo numero de datos.
    for i=1:ndyncat
    categorias(i)=aux(round(naux*i/ndyncat));
    end
    %Agregamos el 0 de la categoria.
    categorias=[0 categorias];
    ncat=length(categorias);
end


%**************************************************************************
%   ESTA PORCION DEL CODIGO COMPUTA LA FRECUENCIA DE OCURRENCIA DE LA
%   PRECIPITACION POR ENCIMA DE UN DADO UMBRAL DADO QUE LA PRECIPITACION
%   PRONOSTICADA ESTA EN ALGUNO DE LOS RANGOS DEFINIDOS POR LA VARIABLE
%   RANGO_LLUVIA.
%**************************************************************************
i_3=(obs == 0);     %Identifico las observaciones que son 0.
for i_cat=1:ncat-1
    i_2=(forecast <= categorias(i_cat+1) & forecast > categorias(i_cat) & obs >= 0);

    aux=sum(i_2);
    
    for i_umb=1:numb
    %Calculo la probabilidad de ocurrencia del fenomeno en funcion de
    %cuanto pronostica la media del ensemble o el control.
    if( umbral(i_umb) > 0);
    i_1=(obs > umbral(i_umb));
    f(i_umb,1)=sum(obs > umbral(i_umb) & forecast == 0);
    elseif( umbral(i_umb) == 0)
    f(i_umb,1)=sum(obs == 0 & forecast == 0);
    i_1=(obs == 0);
    end
    
    aux2=sum(i_1 & i_2);
    aux3=sum(i_3 & i_2);
    if(aux >= min_datos)
    f(i_umb,i_cat+1)=aux2;
    n(i_umb,i_cat+1)=aux;
    else
    f(i_umb,i_cat+1)=NaN;
    n(i_umb,i_cat+1)=NaN;i_1=(obs > umbral(i_umb));
    end
    
    %El primer lugar de f y n permiten calcular la probabilidad residual. Es
    %decir cual es la probabilidad de que cuando el pron???stico es 0 la lluvia
    %este por encima del umbral. Esta probabilidad esta relacionada con la
    %sorpresa. Pero en la sorpresa los pron???sticos que esten por debajo del
    %umbral cuando lloviera por encima del umbral tambien cuentan y en este
    %caso particular solo estamos considerando lo que pasa cuando el
    %pron???stico es estrictamente 0.

    n(i_umb,1)=sum(forecast==0);
 
    end

end


%**************************************************************************
%   FIN DEL CALCULO GUARDAMOS LOS RESULTADOS EN LAS VARIABLES DE SALIDA.
%**************************************************************************

%La estrucutra de las variables f_out y n_out es la siguiente:
%dimension 1: umbrales.
%dimension 2: frecuencia o numero de datos (f o n) para cada categoria.
%dimension 3: pronostico (24-36-48...)
%dimension 4: datos de origen (pluvio-cmorph...)
%dimension 5: region (1-2-3...)

f_out(:,:)=f;
n_out(:,:)=n;
categorias_out=categorias';

else
    
    f_out(:,:)=NaN(numb,ncat);
    n_out(:,:)=NaN(numb,ncat);
    categorias_out=NaN(ncat,1)';
    
    
end %Este es el end del if sobre si la cantidad de datos es mayor que 0.




