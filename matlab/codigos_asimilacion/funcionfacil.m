function [J nablaJ]=funcionfacil(x,x0)

B=eye(length(x));

J=(x-x0)'*B*(x-x0);

nablaJ=B*(x-x0);