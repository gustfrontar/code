import numpy as np

def get_random_pairs( npairs , nprices ) :
    #Definimos pares aleatorios sin hacer un analisis previo de 
    #la cointegracion.
    #TODO: verificar que no este incluyendo 2 veces el mismo par.
    pair_index = np.zeros((npairs,2))

    pair_index[:,0] = np.floor( np.random.rand( npairs ) * nprices ) 

    #Elegimos el segundo asset del par cuidando de no repetir.
    for ii in range( npairs ) :

        second = np.floor( np.random.rand() * nprices )
        while not( second == pair_index[ii,0] ) :
            pair_index[ii,1] = second
            second = np.floor( np.random.rand() * nprices )

    return pair_index.astype(int)     





def get_model( var_1 , var_2 , min_times = 10 , weighted_corr = False , weigth_type = None ) :

    nan_mask = np.logical_not( np.logical_or( np.isnan( var_1 ) , np.isnan( var_2 ) ) )

    var_1 = var_1[nan_mask]
    var_2 = var_2[nan_mask]

    ntimes = len( var_1 ) 

    if ntimes >= min_times : 

       betha , alpha = np.polyfit(var_1, var_2, 1 )

    else :

       betha=np.nan 
       alpha=np.nan

    out=dict()
    out['betha'] = betha
    out['alpha'] = alpha

    return out


def get_spread( var_1 , var_2 , back_time = 25 ) :

    ntimes = len( var_1 ) 

    spread = np.ones( ntimes ) + np.nan

    z_score = np.ones( ntimes ) + np.nan

    betha = np.ones( ntimes ) + np.nan

    alpha = np.ones( ntimes) + np.nan

    #Compute the spread for the entire time series. 
    for ii in range( ntimes ) :
        max_time = ii - 1
        min_time = ii - 1 - back_time 
        if min_time < 0 :
            min_time = 0
        if max_time < 0 :
            max_time = 0
        #print(  var_1[min_time:max_time] , var_2[min_time:max_time] )
        model = get_model( var_1[min_time:max_time] , var_2[min_time:max_time] )
        betha[ii] = model['betha']
        alpha[ii] = model['alpha']

        spread[ii] = var_2[ii] - betha[ii] * var_1[ii] - alpha[ii]

        mean_s = np.mean( spread[min_time:max_time] )
        std_s  = np.std ( spread[min_time:max_time] )

        z_score[ii] = ( spread[ii] - mean_s ) / std_s

    out = dict()
    out['spread'] = spread
    out['z_scores'] = z_score
    out['betha'] = betha
    out['alpha'] = alpha

    return out


def process_pairs( price , pair_index , company , back_time = 25 ) :

    pairs = dict()

    npairs = pair_index.shape[0]

    for ip in range( npairs ) :

       out = get_spread( price[pair_index[ip,0],:] , price[pair_index[ip,1],:] , back_time = back_time ) 

       pairs[str(ip)] = out
       pairs[str(ip)]['Comp1']=company[pair_index[ip,0]]
       pairs[str(ip)]['Comp2']=company[pair_index[ip,1]]
       pairs[str(ip)]['Price1']=price[pair_index[ip,0],:]
       pairs[str(ip)]['Price2']=price[pair_index[ip,1],:]
       pairs[str(ip)]['BackTime'] = back_time

    return pairs   


def trading_strategy( pairs_data , zthresh_open = 0.7 , zthresh_close = 0.1 ) :
    #Apply a simple trading strategy.

    pairs = pairs_data.keys() 

    npairs = len( pairs )

    #Main loop over the pairs. 

    for ip in range( npairs )  :

       print('Processing pair ', ip )
       print('Companies ',pairs_data[str(ip)]['Comp1'],' ',pairs_data[str(ip)]['Comp1'])

       #Time loop.
       price1 = pairs_data[str(ip)]['Price1']
       price2 = pairs_data[str(ip)]['Price2']
       #zscore =  pairs_data[str(ip)]['z_scores']
       betha  = pairs_data[str(ip)]['betha']
       alpha  = pairs_data[str(ip)]['alpha']
       zscore = np.zeros( len(betha) ) + np.nan
       zscore_ini = np.copy( zscore ) 
       zscore_end = np.copy( zscore ) 
       #spread = np.zeros( len(betha) ) + np.nan
       back_time = pairs_data[str(ip)]['BackTime']

       ntimes = len( pairs_data[str(ip)]['Price1'] )

       possition = np.zeros( ntimes )  #1 if open possitive, -1 if open negative, 0 if close.

       possition_return = np.zeros( ntimes ) #Percentual return at possition closure.


       for ii in range(1,ntimes) :
          if possition[ii] == 0  :
            #When the possition is closed we update alpha and betha  
            cbetha = betha[ii]
            calpha = alpha[ii]

          #Recompute zscore .... (fix alpha and betha when we open a possition)
          #update alpha and betha when the possition is closed
          min_time = ii - back_time
          if min_time < 0 :
             min_time = 0
          spread = price2[min_time:ii] - cbetha * price1[min_time:ii] - calpha
          zscore[ii] = ( spread[-1] - np.nanmean( spread ) ) / np.nanstd( spread )

          if ( np.abs( zscore[ii] ) > zthresh_open ) and possition[ii] == 0 :
            #Open possition
            if zscore[ii] > 0 :
               possition[ii:] = 1 
            if zscore[ii] < 0 :
               possition[ii:] = -1
            #Save the prices at the possition opening
            price_open_1 = price1[ii]
            price_open_2 = price2[ii]
            czscore_ini = zscore[ii]


          elif( possition[ii] == 1 and zscore[ii] < zthresh_close ) or ( possition[ii] == -1 and zscore[ii] > -1.0*zthresh_close ):
            #Get the return
            return1 = possition[ii] * ( price1[ii] - price_open_1 ) / price_open_1 
            return2 = possition[ii] * ( price_open_2 - price2[ii] ) / price_open_2
            possition_return[ii] = return1 + return2 
            zscore_ini[ii] = czscore_ini #Store the associated initial zscore for this possition
            zscore_end[ii] = zscore[ii]
            #print( return1 + return2 )
            #Close possition
            if ii + 1 < ntimes - 1 :
               possition[ii+1:] = 0

          
       pairs_data[str(ip)]['Possition'] = possition
       pairs_data[str(ip)]['Possition_return'] = possition_return
       pairs_data[str(ip)]['z_scores_trading'] = zscore #The actual z_score used in the trading strategy
       pairs_data[str(ip)]['z_scores_ini'] = zscore_ini #The zscore at the opening of each possition 
       pairs_data[str(ip)]['z_scores_end'] = zscore_end #The zscore at the closing of each possition


    return pairs_data 

          
          
       







    


