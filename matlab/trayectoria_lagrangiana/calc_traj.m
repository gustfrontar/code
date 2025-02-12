% Valerian Jewtoukoff
% 04/2012
%
%Calculates a backward trajectory using WRF outputs
%
% [xtraj,ytraj,ztraj] = calc_traj(x0,y0,z0,hstart,minstart,hend,minend,dt,dx,dy,umov,vmov)

function [xtraj,ytraj,ztraj] = calc_traj(x0,y0,z0,hstart,minstart,hend,minend,dt,dx,dy,umov,vmov)

timestart = hms2sec(hstart,minstart,0);
timeend = hms2sec(hend,minend,0);

utraj = zeros(size(timestart:dt:timeend));
vtraj = utraj;
wtraj = utraj;

xtraj = utraj;
ytraj = utraj;
ztraj = utraj;

xtraj(1) = x0;
ytraj(1) = y0;
ztraj(1) = z0;

it = 0;

%for i = timestart:dt:timeend-dt
for i = timeend:-dt:timestart+dt
  it = it+1;

  sec = i;
  h = fix(sec/3600);
  sec = sec - 3600*h;
  m = fix(sec/60);

  fname = sprintf('/net/britten/rdsk1/vjewtou/Tornado/wrfout_d02_2008-08-03_%02d_%02d_%02d',h,m,0);

  imin(it) = round(xtraj(it))-5;
  imax(it) = round(xtraj(it))+5;
  jmin(it) = round(ytraj(it))-5;
  jmax(it) = round(ytraj(it))+5;

% Load WRF variables

  u = getwrf1var(fname,'U'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1)-umov;
  v = getwrf1var(fname,'V'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1)-vmov;
  u10 = getwrf1var2(fname,'U10'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1)-umov;
  v10 = getwrf1var2(fname,'V10'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1)-vmov;
  w = getwrf1var(fname,'W'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1);
  zk = getwrf1var(fname,'Z'    ,jmin(it),jmax(it),imin(it),imax(it),-1,-1)*1e3;

% Append the lower level

  uu = zeros(size(u,1)+1,size(u,2),size(u,3));
  vv = zeros(size(u,1)+1,size(u,2),size(u,3));
  ww = zeros(size(u,1)+1,size(u,2),size(u,3));
  zkk = zeros(size(u,1)+1,size(u,2),size(u,3));

  uu(1,:,:) = u10;
  vv(1,:,:) = v10;
  zkk(1,:,:) = 10;
  ww(1,:,:) = squeeze(w(1,:,:));

  for pp = 2:size(u,1)+1
    uu(pp,:,:) = squeeze(u(pp-1,:,:));
    vv(pp,:,:) = squeeze(v(pp-1,:,:));
    zkk(pp,:,:) = squeeze(zk(pp-1,:,:));
    ww(pp,:,:) = squeeze(w(pp-1,:,:));
  end

  u = uu;
  v = vv;
  w = ww;
  zk = zkk;

  x = 1:size(u,2);
  y = 1:size(u,3);
  z = 1:size(u,1);
  [Z,X,Y] = ndgrid(z,x,y);

% Calculate the trajectory

  utraj(it) = interp3(Z,X,Y,u,ztraj(it),xtraj(it)-imin(it),ytraj(it)-jmin(it),'cubic');
  vtraj(it) = interp3(Z,X,Y,v,ztraj(it),xtraj(it)-imin(it),ytraj(it)-jmin(it),'cubic');
  wtraj(it) = interp3(Z,X,Y,w,ztraj(it),xtraj(it)-imin(it),ytraj(it)-jmin(it),'cubic');
  xtraj(it+1) = xtraj(it)-utraj(it)./dx*dt;
  ytraj(it+1) = ytraj(it)-vtraj(it)./dy*dt;

dz(it) = zk(round(ztraj(it))+1,round(xtraj(it))-imin(it),round(ytraj(it))-jmin(it))-zk(round(ztraj(it)),round(xtraj(it))-imin(it),round(ytraj(it))-jmin(it));

  ztraj(it+1) = ztraj(it)-wtraj(it)./dz(it)*dt;

  if ztraj(it+1) < 1
    ztraj(it+1) = 1;
  end
end
