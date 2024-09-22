
function [mascara_out]=identifica_sistema_fun(mascara)

[ny nx]=size(mascara);

sistema=zeros(size(mascara));

ultimo_sistema=1;
for i=1:ny
    for j=1:nx
   
        if(mascara(i,j) == 1)
           tengo_vecino=0;
           for i2=-1:1
               for j2=-1:1
                   %Definiciones extra para imponer las condiciones de
                   %borde.
                   
                   %Condicion de borde ciclica en X
                   if(j+j2 >= 1 & j+j2 <= nx)
                       indexj=j+j2;
                   elseif( j + j2 == 0)
                       indexj=nx; %Condicion de borde ciclica en x.
                   elseif( j + j2 == nx+1)
                       indexj=1;  %Condicione de borde ciclica en x.
                   end
                   
                   %Condicion polar en Y.
                   if(i+i2 >= 1 & i+i2 <= ny)
                    indexi=i+i2;
                   elseif( i+i2 == 0)
                    indexi=1;
                    aux=round(nx/2);
                    indexj=indexj+aux;
                    if(indexj > nx)
                        indexj=indexj-nx;
                    end
                   elseif( i+i2 == ny+1)
                    indexi=ny;
                    aux=round(nx/2);
                    indexj=indexj+aux;
                    if(indexj > nx)
                    indexj=indexj-nx;
                    end
                   end
                   
                   
                   if(sistema(indexi,indexj) > 0 & sistema(i,j)==0 )
                       sistema(i,j)=sistema(indexi,indexj);
                       tengo_vecino=1;
                   end
                   if(sistema(indexi,indexj) > 0 & sistema(i,j) > 0 & sistema(i,j) ~= sistema(indexi,indexj) )
                       auxiliar=max([sistema(indexi,indexj) sistema(i,j)]);
                       sistema(sistema == auxiliar)=min([sistema(indexi,indexj) sistema(i,j)]);
                   end
               end
           end
           if(tengo_vecino==0)
               sistema(i,j)=ultimo_sistema;
               ultimo_sistema=ultimo_sistema+1;
           end
        end
    end
end

mascara_out=sistema;
   
end

















