function  [dist]=distll_fast_fun(alon,alat,blon,blat)

dx=diff_lon_fun(alon,blon);
meanlat=0.5*(alat+blat);
dx=dx*cosd(meanlat)*111000;
dy=(alat-blat)*111000;

dist=sqrt((dy.^2+dx.^2));

end
