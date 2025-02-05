function varargout=getwrf1var(fname,varname,varargin)

% 26/03/2008
% retrieve chosen variables from a wrfout file and unstagger them
% (pressure and temperature can be returned together, one requires the other)
% (idem for zonal and meridional velocities)
% varargin allows specification of bounds for retrieving 
% only a subsample of the variable
% varargin = {ixs,ixe,iys,iye,izs,ize}
% for staggered variables, bounds have been changed so that, 
% either starting index is -1 and then end index 
% does not matter, either starting index is not -1, 
% and then end index is incremented by one to allow unstaggering

if ~isempty(varargin)
   ixs = varargin{1};ixe = varargin{2};
   iys = varargin{3};iye = varargin{4};
   izs = varargin{5};ize = varargin{6};
else
   ixs = -1; ixe = -1; iys = -1; 
   iye = -1; izs = -1; ize = -1; 
end

% 29/11/2010
% modification to allow fname to be a date rather than the name of the file
if (fname(5)=='-') 
  datese = fname; clear('fname'); 
  [dire, fname] = date2stdfile(datese); 
end

if strcmp(varname,'T')            % potential temperature
    traw = getnc(fname,'T',[1 izs iys ixs],[1 ize iye ixe]);
    varargout(1) = {300.+traw};
elseif strcmp(varname,'U')
    uraw = getnc(fname,'U',[1 izs iys ixs],[1 ize iye ixe+1]);
    varargout(1) = {unstagger(uraw,3)}; % w subscript for u in WRF grid
elseif strcmp(varname,'UZON')
    uraw=getnc(fname,'U',[1 izs iys ixs],[1 ize iye ixe+1]);
    u=unstagger(uraw,3);
    vraw=getnc(fname,'V',[1 izs iys ixs],[1 ize iye+1 ixe]);
    v=unstagger(vraw,2);
    % conversion into zonal velocity (frame rotation)
    cosa = getnc(fname,'COSALPHA',[1 iys ixs],[1 iye ixe]); 
    sina = getnc(fname,'SINALPHA',[1 iys ixs],[1 iye ixe]);
    COSA = repmat(cosa,[1 1 size(u,1)]); COSA = shiftdim(COSA,2);
    SINA = repmat(sina,[1 1 size(u,1)]); SINA = shiftdim(SINA,2);
    varargout(1) = {u.*COSA - v.* SINA};
elseif strcmp(varname,'V')
    vraw=getnc(fname,'V',[1 izs iys ixs],[1 ize iye+1 ixe]);
    varargout(1) = {unstagger(vraw,2)}; % w subscript for v in WRF grid
elseif strcmp(varname,'VMERI')
    uraw=getnc(fname,'U',[1 izs iys ixs],[1 ize iye ixe+1]);
    u=unstagger(uraw,3);
    vraw=getnc(fname,'V',[1 izs iys ixs],[1 ize iye+1 ixe]);
    v=unstagger(vraw,2);
    % conversion into meridional velocity (frame rotation)
    cosa = getnc(fname,'COSALPHA',[1 iys ixs],[1 iye ixe]); 
    sina = getnc(fname,'SINALPHA',[1 iys ixs],[1 iye ixe]);
    COSA = repmat(cosa,[1 1 size(u,1)]); COSA = shiftdim(COSA,2);
    SINA = repmat(sina,[1 1 size(u,1)]); SINA = shiftdim(SINA,2);
    varargout(1) = {u.*SINA + v.*COSA};
elseif strcmp(varname,'UZVM')
    uraw=getnc(fname,'U',[1 izs iys ixs],[1 ize iye ixe+1]);
    u=unstagger(uraw,3);
    vraw=getnc(fname,'V',[1 izs iys ixs],[1 ize iye+1 ixe]);
    v=unstagger(vraw,2);
    % conversion into zonal and meridional velocities (frame rotation)
    cosa = getnc(fname,'COSALPHA',[1 iys ixs],[1 iye ixe]);
    sina = getnc(fname,'SINALPHA',[1 iys ixs],[1 iye ixe]);
    COSA = repmat(cosa,[1 1 size(u,1)]); COSA = shiftdim(COSA,2);
    SINA = repmat(sina,[1 1 size(u,1)]); SINA = shiftdim(SINA,2);
    varargout(1) = {u.*COSA - v.* SINA};
    varargout(2) = {u.*SINA + v.*COSA};
 elseif strcmp(varname,'W')
    wraw=getnc(fname,'W',[1 izs iys ixs],[1 ize+1 iye ixe]);
    varargout(1) = {unstagger(wraw,1)};
elseif strcmp(varname,'PH')
    fraw = getnc(fname,'PH',[1 izs iys ixs],[1 ize+1 iye ixe]);
    varargout(1) = {unstagger(fraw,1)};
elseif strcmp(varname,'PHB')
    fraw = getnc(fname,'PHB',[1 izs iys ixs],[1 ize+1 iye ixe]);
    varargout(1) = {unstagger(fraw,1)};
elseif strcmp(varname,'Z')          % altitude
    phs = getnc(fname,'PH',[1 izs iys ixs],[1 ize+1 iye ixe]);
    phbs = getnc(fname,'PHB',[1 izs iys ixs],[1 ize+1 iye ixe]);
    zraw = (phs+phbs)/9810;    % in km to fit Vorcore altitudes
    varargout(1) = {unstagger(zraw,1)};
elseif strcmp(varname,'P')          % pressure
    pp  = getnc(fname,'P',[1 izs iys ixs],[1 ize iye ixe]);
    pb = getnc(fname,'PB',[1 izs iys ixs],[1 ize iye ixe]);  % P and PB are not staggered
    p = pp+pb;
    varargout(1) = {p};
elseif strcmp(varname,'TN')         % temperature
    pp  = getnc(fname,'P',[1 izs iys ixs],[1 ize iye ixe]);
    pb = getnc(fname,'PB',[1 izs iys ixs],[1 ize iye ixe]);  
    p = pp+pb;
    th = getnc(fname,'T',[1 izs iys ixs],[1 ize iye ixe]);   % T is not staggered
    th = 300.+th;
    varargout(1) = {th.*(p/1.013e5).^(287./1005.)};
elseif strcmp(varname,'PTN')          % pressure + temperature
    pp  = getnc(fname,'P',[1 izs iys ixs],[1 ize iye ixe]);
    pb = getnc(fname,'PB',[1 izs iys ixs],[1 ize iye ixe]);
    p = pp+pb;
    varargout(1) = {p};
    th = getnc(fname,'T',[1 izs iys ixs],[1 ize iye ixe]);
    th = 300.+th;
    varargout(2) = {th.*(p/1.013e5).^(287./1005.)};
elseif strcmp(varname,'RHO')        % density
    pp  = getnc(fname,'P',[1 izs iys ixs],[1 ize iye ixe]);
    pb = getnc(fname,'PB',[1 izs iys ixs],[1 ize iye ixe]);
    p = pp+pb;
    th = getnc(fname,'T',[1 izs iys ixs],[1 ize iye ixe]);
    th = 300.+th;
    tn = th.*(p/1.013e5).^(287./1005.);
    varargout(1) = {p./(287.*tn)};
elseif strcmp(varname,'COSI')        % cosalpha - sinalpha
    cosa = getnc(fname,'COSALPHA',[1 iys ixs],[1 iye ixe]);
    sina = getnc(fname,'SINALPHA',[1 iys ixs],[1 iye ixe]);
    varargout(1) = {cosa};
    varargout(2) = {sina};
elseif strcmp(varname,'QVAPOR')        % 
    qv  = getnc(fname,'QVAPOR',[1 izs iys ixs],[1 ize iye ixe]);
    varargout(1) = {qv};
elseif strcmp(varname,'QICE')        % 
    qi  = getnc(fname,'QICE',[1 izs iys ixs],[1 ize iye ixe]);
    varargout(1) = {qi};
elseif strcmp(varname,'QCLOUD')        % 
    qc  = getnc(fname,'QCLOUD',[1 izs iys ixs],[1 ize iye ixe]);
    varargout(1) = {qc};
elseif strcmp(varname,'UGVG')        % geostrophic U - 16 sep 2009, p78
    disp('cette fonction n est pas prete')
    %r   = getwrf1var(fname,'RHO',ixs,ixe,iys,iye,izs,ize) ; 
    %f2d = getnc(fname,'F',[1 iys ixs],[1 iye ixe]);
    %f = zeros(size(r));
    %for ik = 1:size(r,3) 
    %    ik
    %    f(ik,:,:) = f2d;
    %end   
    %disp('Prepared RHO and F')
    %p = getwrf1var(fname,'P',ixs,ixe,iys,iye,izs,ize);
    %z = getwrf1var(fname,'Z',ixs,ixe,iys,iye,izs,ize);
    %[dpdz] = deriv_real_z(p,z);
    %mfm = getnc(fname,'MAPFAC_M',[1 iys ixs],[1 iye ixe]);
    %step = findstep(fname); 
    %[dpdx,dpdy] = gethorizgrad(p,step,mfm);
    %ug = -1./(r.*f).*(dpdy-dzdy.*dpdz);  
    %vg =  1./(r.*f).*(dpdx-dzdx.*dpdz);    
    %varargout(1)=r;
    %varargout(2)=f;
else 
    disp([varname ' is not on of the valid arguments of getwrf1var:'])
    disp('T,U,UZON,V,VMERI,UZVM,W,PH,PHB,P,Z,TN,PTN,RHO or COSI')
    return
end
