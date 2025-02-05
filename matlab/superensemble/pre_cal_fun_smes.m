%**************************************************************************
%       ESTA FUNCION CALCULA 3 RANK HISTOGRAMS EN FUNCION DE LA DISPERSION 
%       LOCAL DEL ENSEMBLE.
%       Script original Noviembre 2006.
%       Corregido BUG datos NAN enero 2007.
%**************************************************************************

function [rank_hist low_std hi_std] = pre_cal_fun(obs,forecast,sm_flag)
%**************************************************************************
%sm_flag decide si el rank histogram resultante va a ser suavizado o no.
%Si es 1 aplico el suavizado, sino no.

a=size(forecast);
ens=a(2);

std_ens=std(forecast,1,2);
i_std=find(std_ens ~= 0 & isnan(std_ens)==0 );

std_lluvia=sort(std_ens(i_std));

low_std=std_lluvia(round(length(std_lluvia)/3));
hi_std=std_lluvia(round(length(std_lluvia)*2/3));

%Vamos a calcular los rank histograms para cada categoria.

i_rank=find(std_ens < low_std);
%Aca estoy considerando los casos en los que son todos 0.
rank_hist(1,:)=rank_fun(obs(i_rank),forecast(i_rank,:));
clear i_rank
i_rank=find(std_ens < hi_std & std_ens >= low_std);
rank_hist(2,:)=rank_fun(obs(i_rank),forecast(i_rank,:));
clear i_rank
i_rank=find(std_ens > hi_std);
rank_hist(3,:)=rank_fun(obs(i_rank),forecast(i_rank,:));

rank_hist(4,:)=squeeze(sum(rank_hist,1));
rank_total=squeeze(sum(rank_hist,2));

for j=1:4
    rank_hist(j,:)=rank_hist(j,:)./rank_total(j);
end

if(sm_flag==1)
    
    [filas columnas]=size(rank_hist);
    for i=1:filas
       for j=2:columnas-1
          rank_hist_sm(i,j)=(rank_hist(i,j-1)+rank_hist(i,j)+rank_hist(i,j+1))/3;   
       end
       %Resuelvo los bordes de forma tal de preserbar la suma de todas las
       %categorias en la version suavizada.
          rank_hist_sm(i,1)=(2*rank_hist(i,1)+rank_hist(i,2))/3;
          rank_hist_sm(i,columnas)=(2*rank_hist(i,columnas)+rank_hist(i,columnas-1))/3;
    end
    %figure
    %plot(rank_hist(1,:))
    %hold on
    %plot(rank_hist_sm(1,:))
    %sum(rank_hist,2)
    %sum(rank_hist_sm,2)
    rank_hist=rank_hist_sm;
end


%**************************************************************************






