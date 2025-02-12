function simul_movie_speedy(x,z,la,lo,d0,h0,h1,step,speed)

u = d0+[(-h0:step:-1) zeros(1,20) (1:step:h1)];
nu = length(u);
lam = min(min(la))-0.1; 
lax = max(max(la));
lom = min(min(lo)); lox = max(max(lo))+0.05;
load coast;


v = VideoWriter(['/Users/alexishannart/Documents/Travail/dada/speedy/visual_speedy_' num2str(round(100000*now)) '.avi']);
v.FrameRate = speed;

open(v);

h = figure('Color','w');

for j=1:2
    if j==1 e = squeeze(x(:,:,u)); else e = squeeze(z(:,:,u)); end
    e = reshape(e,1,numel(e));
    cm(j) =quantile(e,0.05);cx(j)=quantile(e,0.95);
    if cm(j)*cx(j)<0 c = max(abs([cm(j) cx(j)])); cm(j)=-c; cx(j)=c; end   
end


for k = 1:nu
    i = u(k);
    ti = 1+ floor((i-d0)*6/24);
    
    tic;
    for j=1:2 
        subplot(1,2,j);
        axesm('MapProjection','eqdcylin','MapLonLimit',[lom lox],'MapLatLimit',[lam lax],'flatlimit', [lam lax],'Frame','on');box 'off';tightmap;
        if j==1 e = squeeze(x(:,:,i)); else e = squeeze(z(:,:,i)); end
        pcolorm(la, lo, e);
        shading 'interp'
        plotm(lat,long,'-k','Linewidth',2);
        plotm(la(12,12),lo(12,12),'ok','MarkerEdgeColor','k',...
                    'MarkerFaceColor','w','Linewidth',2);
        setm(gca,'Frame','on')
        %axis 'off';
        %imagesc(e);
        caxis([cm(j) cx(j)]); colorbar;% 'bone';
        title(['day ' num2str(ti)])
        %set(gca,'XTick',[],'XTickLabel',[],'YTick',[],'YTickLabel',[])
    end
    %get(h);
    %title(['day ' num2str(ti)])
    frame = getframe(h);
    writeVideo(v,frame);
    disp([k toc]);

end
close(v);

'hi';

end

