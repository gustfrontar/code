function [xa xasmooth,info]=RUN_4DVAR(y,x0b,H,R,B0,ntimes,dt,bst)


%---------------------------------------------
%  apply ETKF to update analysis state
%
%  input:
%       p - total observations
%       yo(1,p) - observations
%       x0b      - background state at the begining of the window.
%       H - observation operator
%       R - observation error covariance
%       B0- matriz inicial de covarianza de los errores del background.
%       ntimes - number of times in the assimilation window.
%       dt -time step to compute the nonlinear evolution and the adjoint
%       model.
%       bst - timep between two observations (in number of time steps).
%  output:
%       xa(dim) - analysis state at the end of the assimilation
%       window.
%       xasmooth(dim,ntimes+1) - analysis state inside the assimilation
%       window. (smoothed analysis for t0 corresponds to the first time
%       component, and xa corresponds to the ntimes+1 time component).
%---------------------------------------------

%Le indicamos a la funcion fminunc que el gradiente esta disponible para la
%minimizacion, entonces la funcion aplica el metodo del gradiente
%conjugado para encontrar el minimo y siempre que lo necesita usa la
%funcion provista para poder calcular el valor de la J y su gradiente.

options = optimset('GradObj','on');
%options = optimset('TolX',1e-5);
%options = optimset('TolFun',1e-5);
%options=optimset();

invB0=inv(B0);
invR=inv(R);

%Usamos la funcion fminunc de matlab que es una rutina de minimizacion que
%hace uso de la funcion que vamos a minimizar y su gradiente. Lo que le
%pasamos como primer argumento es una rutina que en funcion del valor de x
%calcula la funcion de costo y el gradiente de la funcion de costo.
[minX,minJ,minnablaJ,info] = fminunc('j_and_gradj',x0b,options,x0b,y,H,invR,invB0,dt,bst,ntimes);


%minX es el x0 que produce el minimo de la funcion de costo. Ahora lo que
%tenemos que hacer es integrar desde este x0 hasta xf para obtener el
%analisis en el tiempo final de la ventana.

x=minX;
xasmooth(1,:)=minX;  %Este es el analysis smooth obtenido al inicio de la ventana de asimilacion.
for ii=1:ntimes
   for it=1:bst
       x=stepit(x,dt);
   end
   xasmooth(ii+1,:)=x; 
end

xa=x;

return

