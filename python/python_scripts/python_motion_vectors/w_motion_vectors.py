import numpy as np
import w_motion_vectors as mv
from pylab import *

nx = 50
ny = 50

t0=np.random.rand(nx,ny)

t1=np.random.rand(nx,ny)

t1[0:nx-3,0:ny-1]=t0[2:nx-1,0:ny-1]
t1[nx-1,:]=0.0
t1[nx-2,:]=0.0

t1[:,0:25]=t0[:,0:25]


dt = 1
dx = 1
desp_max = 4

sigma=4
sigma_threshold=8


u_motion,v_motion,correlation = mv.motion_vector(field_t0=t0,field_t1=t1,dt=dt,dx=dx,sigma=sigma,sigma_threshold=sigma_threshold,nx=nx,ny=ny,desp_max=desp_max)

print v_motion[10,:]

figure()
Q = quiver(  u_motion , v_motion, pivot='mid', color='r', units='inches' )
#qk = quiverkey(Q, 0.5, 0.03, 1, r'$1 \frac{m}{s}$', fontproperties={'weight': 'bold'})
axis([0, nx, 0, ny])


show()




