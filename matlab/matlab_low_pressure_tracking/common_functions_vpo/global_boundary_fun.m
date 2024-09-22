            
function [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny)

                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                       %Tenemos que considerar ademas la posibilidad de que
                       %al corregir la longitud se nos valla de rango.
                       if(indexj > nx)
                       indexj=indexj-nx;
                       end
                   end
                   if(indexi < 1)
                   indexi=2-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                       %Tenemos que considerar ademas la posibilidad de que
                       %al corregir la longitud se nos valla de rango.
                       if(indexj > nx)
                       indexj=indexj-nx;
                       end
                   end
                  
end