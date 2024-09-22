% This function provides latitudes on a Gaussian grid from the
% number of latitude lines.
%
% the calling format is:
%               [xlat,dlat,sinc]=gauss2lats(nlat)
%     where: input is nlat = the number of latitude lines, e.g 94 in a T62 grid
%                             with 192 longitudes and 94 latitudes
%            outputs are:
%                      xlat = the latitudes
%                      dlat = latitude spacing
%                      sinc = sine of colatitudes (cosine of latitudes)
% 
% Adapted for Matlab from the NCAR Fortran program by Tom Holt, 23/10/2002.
%
% Possible problems:
%          1) The output may appear slightly different from other
%          estimates. I am indebted to Lee Panetta who provided the
%          following test results (21.10.2003), comparing Gauss2lats on a
%          G4 Mac with another algorithm on a Cray J90:
%                   agreement to 15 decimal places except at endpoints
%                   where agreement was to 13 decimal places.
%          2) The routine may not converge to a solution on some grids.

function [xlat,dlat,sinc]=gauss2lats(nlat)
acon=180.0/pi;

% convergence criterion for iteration of cos latitude
xlim=1.0e-7;

% initialise arrays
for i=1:720
    cosc(i)=0.;
    gwt(i)=0.;
    sinc(i)=0.;
    colat(i)=0.;
    wos2(i)=0.;
end

% the number of zeros between pole and equator
nzero=nlat/2;

% set first guess for cos(colat)
for i=1:nzero;
    cosc(i)=sin((i-0.5)*pi/nlat+pi*0.5);
end

% constants for determining the derivative of the polynomial
fi=nlat;
fi1=fi+1.0;
a=fi*fi1/sqrt(4.0*fi1*fi1-1.0);
b=fi1*fi/sqrt(4.0*fi*fi-1.0);

%loop over latitudes, iterating the search for each root
for i=1:nzero
    % determine the value of the ordinary Legendre polynomial for the current guess root
    g=gord(nlat,cosc(i));
    % determine the derivative of the polynomial at this point
    gm=gord(nlat-1,cosc(i));
    gp=gord(nlat+1,cosc(i));
    gt=(cosc(i)*cosc(i)-1.0)/(a*gp-b*gm);
    % update the estimate of the root
    delta=g*gt;
    cosc(i)=cosc(i)-delta;
 
    % if convergence criterion has not been met, keep trying
    while abs(delta) > xlim
        g=gord(nlat,cosc(i));
        gm=gord(nlat-1,cosc(i));
        gp=gord(nlat+1,cosc(i));
        gt=(cosc(i)*cosc(i)-1.0)/(a*gp-b*gm);
        delta=g*gt;
        cosc(i)=cosc(i)-delta;   
    end
    % determine the Gaussian weights
    c=2.0*(1.0-cosc(i)*cosc(i));
    d=gord(nlat-1,cosc(i));
    d=d*d*fi*fi;
    gwt(i)=c*(fi-0.5)/d;
end

% determine the colatitudes and sin(colat) and weights over sin**2
for i=1:nzero
    colat(i)=acos(cosc(i));
    sinc(i)=sin(colat(i));
    wos2(i)=gwt(i)/(sinc(i)*sinc(i));
end

% if nlat is odd, set values at the equator
if mod(nlat,2) ~= 0 
    i=nzero+1;
    cosc(i)=0.0;
    c=2.0;
    d=gord(nlat-1,cosc(i));
    d=d*d*fi*fi;
    gwt(i)=c*(fi-0.5)/d;
    colat(i)=pi*0.5;
    sinc(i)=1.0;
    wos2(i)=gwt(i);
end

% determine the southern hemisphere values by symmetry
for i=nlat-nzero+1:nlat
    cosc(i)=-cosc(nlat+1-i);
    gwt(i)=gwt(nlat+1-i);
    colat(i)=pi-colat(nlat+1-i);
    sinc(i)=sinc(nlat+1-i);
    wos2(i)=wos2(nlat+1-i);
end

ylat=90.;

% calculate latitudes and latitude spacing
for i=1:nzero
    xlat(i)=acos(sinc(i))*acon;
    dlat(i)=xlat(i)-ylat;
    ylat=xlat(i);
end




% This function calculates the value of an ordinary Legendre polynomial at a latitude.
%          inputs are:
%                n = the degree of the polynomial
%                x = cos(colatitude)
%         outputs are:
%              ggg = the value of the Legendre polynomial of degree n at 
%                    latitude asin(x)
%
function [ggg]=gord(n,x)

% determine the colatitude
colat=acos(x);

c1=sqrt(2.0);

for i=1:n
    c1=c1*sqrt(1.0-1.0/(4*i*i));
end

fn=n;
ang=fn*colat;
s1=0.0;
c4=1.0;
a=-1.0;
b=0.0;

for k=0:2:n
    if k==n 
        c4=0.5*c4;
    end
    s1=s1+c4*cos(ang);
    a=a+2.0;
    b=b+1.0;
    fk=k;
    ang=colat*(fn-fk-2.0);
    c4=(a*(fn-b+1.0)/(b*(fn+fn-a)))*c4;
end
ggg=s1*c1;