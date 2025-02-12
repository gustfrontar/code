function main_speedy()


[x latloc lonloc] = read_speedy(15); % reads the files 
xa = process_speedy(x); % gets the anomaly
z = filter(ones(1,60)/60,1,x,[],3); % 15 days moving average
q = squeeze(z(12,12,:)); % this is the series in Zurich
d0 = find(q==max(q)); % finds the maximum 
simul_movie_speedy(xa,za,latloc,lonloc,d0,100,50,1,15); % generates a movie before and after the maximum

end

