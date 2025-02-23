function [u_new, v_new, h_new] = lax_wendroff(dx, dy, dt, g, u, v, h, ...
					      u_tendency, v_tendency);
% This function performs one timestep of the Lax-Wendroff scheme
% applied to the shallow water equations

% First work out mid-point values in time and space
uh = u.*h;
vh = v.*h;


h_mid_xt = 0.5.*(h(2:end,:)+h(1:end-1,:)) ...
  -(0.5*dt/dx).*(uh(2:end,:)-uh(1:end-1,:));
h_mid_yt = 0.5.*(h(:,2:end)+h(:,1:end-1)) ...
  -(0.5*dt/dy).*(vh(:,2:end)-vh(:,1:end-1));

Ux = uh.*u+0.5.*g.*h.^2;
Uy = uh.*v;
uh_mid_xt = 0.5.*(uh(2:end,:)+uh(1:end-1,:)) ...
  -(0.5*dt/dx).*(Ux(2:end,:)-Ux(1:end-1,:));
uh_mid_yt = 0.5.*(uh(:,2:end)+uh(:,1:end-1)) ...
  -(0.5*dt/dy).*(Uy(:,2:end)-Uy(:,1:end-1));

Vx = Uy;
Vy = vh.*v+0.5.*g.*h.^2;
vh_mid_xt = 0.5.*(vh(2:end,:)+vh(1:end-1,:)) ...
  -(0.5*dt/dx).*(Vx(2:end,:)-Vx(1:end-1,:));
vh_mid_yt = 0.5.*(vh(:,2:end)+vh(:,1:end-1)) ...
  -(0.5*dt/dy).*(Vy(:,2:end)-Vy(:,1:end-1));

% Now use the mid-point values to predict the values at the next
% timestep
h_new = h(2:end-1,2:end-1) ...
  - (dt/dx).*(uh_mid_xt(2:end,2:end-1)-uh_mid_xt(1:end-1,2:end-1)) ...
  - (dt/dy).*(vh_mid_yt(2:end-1,2:end)-vh_mid_yt(2:end-1,1:end-1));

Ux_mid_xt = uh_mid_xt.*uh_mid_xt./h_mid_xt + 0.5.*g.*h_mid_xt.^2;
Uy_mid_yt = uh_mid_yt.*vh_mid_yt./h_mid_yt;
uh_new = uh(2:end-1,2:end-1) ...
  - (dt/dx).*(Ux_mid_xt(2:end,2:end-1)-Ux_mid_xt(1:end-1,2:end-1)) ...
  - (dt/dy).*(Uy_mid_yt(2:end-1,2:end)-Uy_mid_yt(2:end-1,1:end-1)) ...
  + dt.*u_tendency.*0.5.*(h(2:end-1,2:end-1)+h_new);

Vx_mid_xt = uh_mid_xt.*vh_mid_xt./h_mid_xt;
Vy_mid_yt = vh_mid_yt.*vh_mid_yt./h_mid_yt + 0.5.*g.*h_mid_yt.^2;
vh_new = vh(2:end-1,2:end-1) ...
  - (dt/dx).*(Vx_mid_xt(2:end,2:end-1)-Vx_mid_xt(1:end-1,2:end-1)) ...
  - (dt/dy).*(Vy_mid_yt(2:end-1,2:end)-Vy_mid_yt(2:end-1,1:end-1)) ...
  + dt.*v_tendency.*0.5.*(h(2:end-1,2:end-1)+h_new);
u_new = uh_new./h_new;
v_new = vh_new./h_new;

