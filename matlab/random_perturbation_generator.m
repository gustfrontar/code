clear all
close all

%Generador de perturbaciones pseudo al azar.

%Rango de fechas a partir del cual se podran elegir campos.
fecha_inicial='1982020100';
fecha_final='1982043000';

ROOT=['../DATA/nature_ncep2'];
PERT=['../DATA/nature_ncep2/mult_inf/'];
mkdir(PERT);

umbralcercania=7;   %Distancia minima en dias entre dos campos para generar una perturbacion.


nperts=1000;


inidn=datenum(fecha_inicial,'yyyymmddHH');
enddn=datenum(fecha_final,'yyyymmddHH');

pares=NaN(nperts,2);

nvars=29; 
nx_s=96;
ny_s=48;
nz_s=7;


fprintf('Generando los pares');
for ipert=1:nperts

    
    %Tomo un numero aleatorio entre las fechas inicial y final.
    aleatorio=rand;
    aleatorio=aleatorio*(enddn-inidn)+inidn;
    %Lo llevo a las 6 horas mas cercanas.
    
    aleatorio=round(aleatorio*4)/4;
    
    %Ahora elijo un numero aleatorio que se corresponda con la misma hora
    %que el anterior y que ademas este al menos a 7 dias de distancia.
    
    cont=true;
    while(cont)
       
       aleatorio2=rand;
       aleatorio2=aleatorio2*(enddn-inidn)+inidn;
       aleatorio2=round(aleatorio2*4)/4;

       if( aleatorio2-floor(aleatorio2) == aleatorio-floor(aleatorio))
          if( abs(aleatorio2-aleatorio) > 7)
          %Si se cumple que la distancia en tiempo es de al menos 7 dias y ademas 
          %que ambos corresponden a la misma hora entonces chequeo que este
          %valor no coincida con alguno de los ya elegidos.
             iguales=false;
             for ii=1:ipert-1
                 if((pares(ii,1)==aleatorio & pares(ii,2)==aleatorio2) | (pares(ii,1)==aleatorio2 & pares(ii,2)==aleatorio))
                 iguales=true;
                 end
             end
             if(~iguales)
                 cont=false;
                 %Si no es igual a ninguno de los anteriores genero el par
                 %correspondiente.
                 pares(ipert,1)=aleatorio;
                 pares(ipert,2)=aleatorio2;
             end
                 
          end
       end
    
    
    end
   
end

%Hasta aca genere los pares con las fechas que voy a resptar para generar
%las perturbaciones.

%Ahora hago un loop sobre los "pares" y para cada uno leo los archivos
%correspondientes, los resto y los guardo en un archivo con el numero de
%perturbacion.

for ipert=1:nperts
    
   
    %obtengo las fechas
    fechaa=datestr(pares(ipert,1),'yyyymmddHH');
    fechab=datestr(pares(ipert,2),'yyyymmddHH');
    
    filea=[ROOT '/' fechaa '.grd'];
    fileb=[ROOT '/' fechab '.grd'];
    
    
    na=fopen(filea,'r','b');
    nb=fopen(fileb,'r','b');

   
   tmpa=NaN(ny_s,nx_s,nvars);
   tmpb=NaN(ny_s,nx_s,nvars);

   
   if(na ~= -1)
     for ivars=1:nvars
     tmpa(:,:,ivars)=fread(na,[nx_s ny_s],'single')';
     end
     fclose(na);
   end
   if(nb ~= -1)
     for ivars=1:nvars
     tmpb(:,:,ivars)=fread(nb,[nx_s ny_s],'single')';
     end
     fclose(nb);
   end
   
   %Calculo la perturbacion y la escribo en un archivo.
   perturbation=tmpa-tmpb;
     
     strpert=num2str(10000+ipert);
     strpert=strpert(2:end);
     
     
     pert_file=[PERT '/' strpert '.grd'];
     np=fopen(pert_file,'w','b');
     for ivars=1:nvars
     fwrite(np,perturbation(:,:,nvars)','real*4');
     end
     

     fclose(np);
     
   
end