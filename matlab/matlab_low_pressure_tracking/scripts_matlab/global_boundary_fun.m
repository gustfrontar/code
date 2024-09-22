            
function [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny)

                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny-1);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   if(indexi < 1)
                   indexi=1-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                  
end