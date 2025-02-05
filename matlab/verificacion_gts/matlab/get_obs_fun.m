function [OBS ESTACIONES FECHAS]=get_obs_fun(ini_date,end_date,data_path)
%==========================================================================
%USO [OBS ESTACIONES FECHAS]=GET_OBS_FUN(INI_DATE,END_DATE,DATA_PATH)
% INI-DATE es la fecha de inicio del periodo.
% END-DATE la fecha de fin
% DATA_PATH es el path donde estan los archivos synop de ogimet.
% La funcion lee los archivos del periodo indicado y genera una array con
% las siguientes dimensiones.
% OBS(estacion,dia,hora_del_dia,variable)
% las horas del dia son 4 00, 06, 12 y 18.
% las variables son 5, T, TD, U, V y SLP.
% las estaciones varian de acuerdo al caso... sera las que encuentre en el
% periodo indicado.
% el dia esta referido al periodo indicado, ini_date sera el dia 1 y asi
% siguiendo.
% Tambien se genera el array ESTACIONES
% ESTACIONES(estaciones,datos_estaciones) la primera columna es el numero
% de estacion, la segunda la latitud, la tercera la longitud y la cuarta la
% altura de la estacion. 
% Finalmente se genera el array fechas que es un vector de N componentes
% donde N es el largo del periodo en dias. Cada componente contiene un
% valor numerico que representa la fecha en el formato YYYYMMDDHH.
% La funcion realiza un pequenio control de calidad se chequean valores
% fisicamente irreales de las variables (ver limites seteados mas abajo) y
% se controla que el valor de las variables no exceda un cierto umbral
% alrededor de la media (medido en sigmas, tambien seteado mas abajo).
%==========================================================================
% Juan Ruiz - 2009
%*************************************************************************
%PARAMETROS MODIFICABLES

umbral_sigma=3;     %Si una observacion supera 3 desvios standard la elimino.
umbral_altura=3000; %Si la altura de una estacion supera un determinado umbral la elimino.
umbral_altura_slp=700; %Si una estacion supera esta altura elimino el dato de SLP.
undef=-999;            %El valor de undef de los synop ogimet.
%Definimos algunos rangos aceptables para las variables.
maxt=50;
mint=-30;
maxtd=30;
mintd=-50;
maxu=50;
minu=-50;
maxv=maxu;
minv=minu;
maxslp=1060;
minslp=900;

n_est_ini=1000;

nvariables=7;
nhoras=4;
knottometpersec=0.514444;
%**************************************************************************

ESTACIONES=[];
FECHAS=[];

n_archivos=0;        %Numero de archivos encontrados.

%Archivos de salida:

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)


%Genero un n??mero que identifica la fecha de hoy.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);

ntiempos=end_date_num-ini_date_num+1;

OBS=NaN(n_est_ini,ntiempos,nhoras,nvariables);
%COMIENZA EL CICLO EN TIEMPO.
while (date_num <= end_date_num);
    
    FECHAS(date_num-ini_date_num+1)=str2num(datestr(date_num,'yyyymmddHH'));
    
    %Genero un vector con las componentes de la fecha. yyyy mm dd
    %genero un n??mero con la fecha en formato yyyymmdd
    %lo paso a string y lo uso para generar el nombre del archivo que vamos
    %a abrir.
    for ihora=1:4  %Voy completando las matrices hora a hora.
        horastr{1}='00';horastr{2}='06';horastr{3}='12';horastr{4}='18';
    
    open_file=(strcat(data_path,'SYNOP_OGIMET_',datestr(date_num,'yyyymmdd'),horastr{ihora}));
    
    %Verifico la existencia del archivo.
    narch=fopen(open_file);
    
    %Si existe leo los datos que contiene.
    if(narch ~= -1);
    fclose(narch);
    n_archivos=n_archivos+1;
    [gts_data]=load(open_file);
    [nest columnas]=size(gts_data);
    gts_lat=gts_data(:,3);          %Latitud de las estaciones que lei
    gts_lon=gts_data(:,4);          %Longitud de las estaciones que lei
    gts_number=gts_data(:,2);       %Numero de estacion de las estaciones que lei
    gts_topo=gts_data(:,5);         %Altura de las estaciones que lei.
    gts_data(gts_data==undef)=NaN;  %Paso los undef a NaN para no tener problemas.
    gts_u=-gts_data(:,11).*sind(gts_data(:,10))*knottometpersec; %Paso de direccion e intensidad a U y V.
    gts_v=-gts_data(:,11).*cosd(gts_data(:,10))*knottometpersec;
    gts_vel=gts_data(:,11)*knottometpersec;
    gts_dir=gts_data(:,10);
    
    
        %Si no es el primer dia entonces busco las estaciones una por una e
        %intento buscar en que columna tengo que ubicar el nuevo dato. 
        %Primero creo una nueva columna de NaN en la matriz.
        columna_nueva=date_num-ini_date_num+1;

        %Si la variable estaciones no existe la creo en este punto.
        if(isempty(ESTACIONES))
        ESTACIONES(1,:)=[gts_number(1) gts_lat(1) gts_lon(1) gts_topo(1)];
        end
       
        for iest=1:nest;
            
        fila=find(ESTACIONES(:,2)==gts_lat(iest) & ESTACIONES(:,3)==gts_lon(iest));

        if(isempty(fila))
            %No encontre la estacion entonces tengo que crear una nueva
            %fila en las matrices
            fila_nueva=length(ESTACIONES(:,1))+1;

            %Agrego la nueva fila en todas las horas y variables.
            
            OBS(fila_nueva,:,:,:)=NaN;
            
            ESTACIONES(fila_nueva,:)=[gts_number(iest) gts_lat(iest) gts_lon(iest) gts_topo(iest)];

            %Finalmente agrego los datos solo en la hora correspondiente.
            OBS(fila_nueva,columna_nueva,ihora,1)=gts_data(iest,8); %TEMPERATURA
            OBS(fila_nueva,columna_nueva,ihora,2)=gts_data(iest,9); %TD
            OBS(fila_nueva,columna_nueva,ihora,3)=gts_u(iest); %U
            OBS(fila_nueva,columna_nueva,ihora,4)=gts_v(iest); %V
            OBS(fila_nueva,columna_nueva,ihora,5)=gts_data(iest,7); %SLP
            OBS(fila_nueva,columna_nueva,ihora,6)=gts_vel(iest);  %INTENSIDAD DEL VIENTO
            if(gts_vel(iest) > 0)
            OBS(fila_nueva,columna_nueva,ihora,7)=gts_dir(iest);  %DIRECCION DEL VIENTO
            else
            OBS(fila_nueva,columna_nueva,ihora,7)=NaN;
            end 
            
        else
            %Encontre la estacion agrego el dato en la fila correspondiente
            OBS(fila,columna_nueva,ihora,1)=gts_data(iest,8); %TEMPERATURA
            OBS(fila,columna_nueva,ihora,2)=gts_data(iest,9); %TD
            OBS(fila,columna_nueva,ihora,3)=gts_u(iest); %U
            OBS(fila,columna_nueva,ihora,4)=gts_v(iest); %V
            OBS(fila,columna_nueva,ihora,5)=gts_data(iest,7); %SLP
            OBS(fila,columna_nueva,ihora,6)=gts_vel(iest); %INTENSIDAD DEL VIENTO
            if(gts_vel(iest) > 0)
            OBS(fila,columna_nueva,ihora,7)=gts_dir(iest); %DIRECCION DEL VIENTO
            else
            OBS(fila,columna_nueva,ihora,7)=NaN;
            end
        end
        
       
        end
    end
         
        
    end

 date_num=date_num+1;
        
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!



if(n_archivos > 0)  %Si la cantidad de archivos encontrados es mayor que 0.

OBS(OBS==undef)=NaN;

%Me quedo con la cantidad maxima de estaciones.
nest=length(ESTACIONES(:,1));
OBS=OBS(1:nest,:,:,:);

%==========================================================================
% APLICO LAS RESTRICCIONES DE ALTURA
%==========================================================================


%Umbral de altura elimino estaciones que estan muy alto.
index_aux=ESTACIONES(:,4) < umbral_altura;

OBS=OBS(index_aux,:,:,:);
ESTACIONES=ESTACIONES(index_aux,:);

%Umbral de altura para SLP no considero las obs de SLP para estacions muy
%altas.
index_aux=ESTACIONES(:,4) > umbral_altura_slp;

OBS(index_aux,:,:,5)=NaN;

%==========================================================================
% APLICO UN CONTROL DE CALIDAD SENCILLO BASADO EN LA ESTANDARIZACION.
%==========================================================================

%Primero chequeo los rangos basicos de las variables.
%T
aux=OBS(:,:,:,1);
aux(aux < mint | aux > maxt)=NaN;
OBS(:,:,:,1)=aux;
%TD
aux=OBS(:,:,:,2);
aux(aux < mintd | aux > maxtd)=NaN;
OBS(:,:,:,2)=aux;
%U
aux=OBS(:,:,:,3);
aux(aux < minu | aux > maxu)=NaN;
OBS(:,:,:,3)=aux;
%V
aux=OBS(:,:,:,4);
aux(aux < minv | aux > maxv)=NaN;
OBS(:,:,:,4)=aux;
%SLP
aux=OBS(:,:,:,5);
aux(aux < minslp | aux > maxslp)=NaN;
OBS(:,:,:,5)=aux;
%DIR
aux=OBS(:,:,:,7);
aux(aux < 0   |  aux > 360)=NaN;
OBS(:,:,:,7)=aux;


%Las observaciones que excedan un determinado valor de SIGMA en la variable
%estandarizada seran eliminadas.

STD_OBS=squeeze(nanstd(OBS,[],2));
MEAN_OBS=squeeze(nanmean(OBS,2));

OBSS=NaN(size(OBS));
for i=1:ntiempos
   OBSS(:,i,:,:)=(squeeze(OBS(:,i,:,:))-MEAN_OBS)./STD_OBS;   
end

OBS(OBSS> umbral_sigma)=NaN;

end
