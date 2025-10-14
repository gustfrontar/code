import pickle as pkl
import numpy as np

a=dict()

a['pepe']=np.random.randn(10,10)

filename = 'pepe.npz'
np.savez(filename,a,allow_pickle=True)

b=np.load(filename,allow_pickle=False)


print(a)
print(b['arr_0'])


