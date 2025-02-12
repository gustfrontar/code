'reinit'
'open xcorr.ctl'
'set z 4'
'set xlopts 1 4 0.18'
'set ylopts 1 4 0.2'
'set gxout shaded'
'set mproj off'
'set parea 1 10 1.4 7.8'
'set grads off'
'd ps'
'cbarn'
'draw title SPEEDY BGERR HORIZONTAL CORREL NMC_METHOD (PS)'
'draw xlab LENGTH SCALE [GRID]'
'draw ylab LATITUDE [DEGREE]'
'printim xcorr_ps.gif gif white x480 y360'
'c'
k=1
while(k<8)
if(k=1)
sigma=0.95
endif
if(k=2)
sigma=0.835
endif
if(k=3)
sigma=0.685
endif
if(k=4)
sigma=0.51
endif
if(k=5)
sigma=0.34
endif
if(k=6)
sigma=0.2
endif
if(k=7)
sigma=0.08
endif
'set grads off'
'd u(z='k')'
'cbarn'
'draw title SPEEDY BGERR HORIZONTAL CORREL NMC_METHOD (U,sigma='sigma')'
'draw xlab LENGTH SCALE [GRID]'
'draw ylab LATITUDE [DEGREE]'
'printim xcorr_u'k'.gif gif white x480 y360'
'c'
'set grads off'
'd v(z='k')'
'cbarn'
'draw title SPEEDY BGERR HORIZONTAL CORREL NMC_METHOD (V,sigma='sigma')'
'draw xlab LENGTH SCALE [GRID]'
'draw ylab LATITUDE [DEGREE]'
'printim xcorr_v'k'.gif gif white x480 y360'
'c'
'set grads off'
'd t(z='k')'
'cbarn'
'draw title SPEEDY BGERR HORIZONTAL CORREL NMC_METHOD (T,sigma='sigma')'
'draw xlab LENGTH SCALE [GRID]'
'draw ylab LATITUDE [DEGREE]'
'printim xcorr_t'k'.gif gif white x480 y360'
'c'
'set grads off'
'd q(z='k')'
'cbarn'
'draw title SPEEDY BGERR HORIZONTAL CORREL NMC_METHOD (Q,sigma='sigma')'
'draw xlab LENGTH SCALE [GRID]'
'draw ylab LATITUDE [DEGREE]'
'printim xcorr_q'k'.gif gif white x480 y360'
'c'
k=k+1
endwhile


