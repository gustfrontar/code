'reinit'
'open stdev.ctl'
'set z 4'
'set gxout shaded'
'set xlopts 1 4 0.2'
'set ylopts 1 4 0.18'
'set grads off'
'd ps'
'cbarn'
'draw title SPEEDY BGERR STDEV NMC_METHOD (PS)'
'printim stdev_ps.gif gif white x480 y360'
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
'draw title SPEEDY BGERR STDEV NMC_METHOD (U,sigma='sigma')'
'printim stdev_u'k'.gif gif white x480 y360'
'c'
'set grads off'
'd v(z='k')'
'cbarn'
'draw title SPEEDY BGERR STDEV NMC_METHOD (V,sigma='sigma')'
'printim stdev_v'k'.gif gif white x480 y360'
'c'
'set grads off'
'd t(z='k')'
'cbarn'
'draw title SPEEDY BGERR STDEV NMC_METHOD (T,sigma='sigma')'
'printim stdev_t'k'.gif gif white x480 y360'
'c'
'set grads off'
'd q(z='k')'
'cbarn'
'draw title SPEEDY BGERR STDEV NMC_METHOD (Q,sigma='sigma')'
'printim stdev_q'k'.gif gif white x480 y360'
'c'
k=k+1
endwhile


