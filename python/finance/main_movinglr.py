import numpy as np
import os
from read_data import csv2npz
import tools as tl
import plot_tools as ptl

data_path = '/home/jruiz/share/DATA/PRICES/'
plot_path = '/home/jruiz/share/DATA/PRICES/plots/'
ini_date  = '2000-01-01'
end_date  = '2023-12-31'
var_type  = 'close'
industry_type = 'Oil, Gas & Consumable Fuels'
zthresh_open  = 2.0
zthresh_close = 0.1 
npairs = 10
random_seed = 10

np.random.seed( random_seed )


npz_file = data_path+f"oil_{ini_date}-{end_date}_{var_type}.npz"
if not os.path.exists(npz_file) :

   csv2npz(init_date=ini_date,end_date=end_date,
      var_type=var_type,
      folder= data_path,
      industry_type=industry_type)

#Read the data from the NPZ file
data=np.load(npz_file)

day=data['day']
price=data['price']
company=data['company']

print('Companies to be used')
print( company )

#Select random pairs. 
pair_index = tl.get_random_pairs( npairs , len(company) ) 

print('These are the pairs')
print( pair_index )


#Compute z-score
pairs_data = tl.process_pairs( price , pair_index , company , back_time = 25 )

print( pairs_data['1'] )


pairs_data = tl.trading_strategy( pairs_data , zthresh_open = zthresh_open , zthresh_close = zthresh_close )


print( pairs_data['1'] )

npairs=len( pairs_data.keys() )

for ip in range( npairs ) :

   ptl.plot_pair( pairs_data[str(ip)] , plot_path , show = False ) 


