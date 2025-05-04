import numpy as np

def get_random_pairs( npairs , nprices ) :
    #Definimos pares aleatorios sin hacer un analisis previo de 
    #la cointegracion.
    #TODO: verificar que no este incluyendo 2 veces el mismo par.
    pair_index = np.zeros((npairs,2))

    pair_index[:,0] = np.round( np.random.rand( npairs ) * nprices ) 

    #Elegimos el segundo asset del par cuidando de no repetir.
    for ii in range( npairs ) :

        second = np.round( np.random.rand() * nprices )
        while not( second == pair_index[ii,0] ) :
            pair_index[ii,1] = second
            second = np.round( np.random.rand() * nprices )

    return pair_index     





def get_model( var_1 , var_2 , min_times = 10 , weighted_corr = False , weigth_type = None ) :

    nan_mask = not ( np.nan( var_1 ) or np.nan( var_2 ) )

    var_1 = var_1[nan_mask]
    var_2 = var_2[nan_mask]

    ntimes = len( var_1 ) 

    if ntimes >= min_times : 

       betha , alpha = np.polyfit(var_1, var_2, 1 )

    else :

       betha=np.nan , alpha=np.nan

    out=dict()
    out['betha'] = betha
    out['alpha'] = alpha

    return out


def get_spread( var_1 , var_2 , back_time = 25 ) :

    ntimes = len( var_1 ) 

    spread = np.ones( ntimes ) + np.nan

    z_norm = np.ones( ntimes ) + np.nan

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
        model = get_model( var_1[min_time:max_time] , var_2[min_time:max_time] )
        betha[ii] = model['betha']
        alhpa[ii] = model['alpha']

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

       




    


