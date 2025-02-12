function xxa = process_speedy(xx)

%load('/Users/alexishannart/Documents/Travail/dada/speedy/data1.mat')
%xx = squeeze(data(:,:,15,1:42340))-273;
xx = xx(:,:,1:42340);
xxa = 0*xx;
for i=1:23
    for j=1:23
        x = squeeze(xx(i,j,:));
        z = filter(ones(1,60)/60,1,x);
        z(1:60) = NaN; z(1:30) = []; z = [z;NaN*ones(30,1)];
        x = reshape(x,4*365,29);
        z = reshape(z,4*365,29);
        zm = nanmean(z,2); zs = nanstd(z,1,2);
        %za = (z-zm*ones(1,29))./(zs*ones(1,29));
        za = (z-zm*ones(1,29));
        xa = (x-zm*ones(1,29));
        xa = reshape(xa,4*365*29,1);
        za = reshape(za,4*365*29,1);
        za = za/std(za);
        xxa(i,j,:) = xa;
        disp([i j]);
        'hi';
    end
end

'hi';

end

