import numpy as np
import motion_vectors as mv
from pylab import *

nx = 200
ny = 200

t0=np.random.rand(nx,ny)

t1=np.random.rand(nx,ny)

dt = 1

box_size = 20
desp_max = 10


u_motion,v_motion,correlation = mv.motion_vector(t0,t1,box_size,dt,desp_max,nx,ny)



figure()
Q = quiver(  u_motion , v_motion, pivot='mid', color='r', units='inches' )
qk = quiverkey(Q, 0.5, 0.03, 1, r'$1 \frac{m}{s}$', fontproperties={'weight': 'bold'})
axis([0, nx, 0, ny])


show()




