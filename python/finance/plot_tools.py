import numpy as np
import matplotlib.pyplot as plt


def possition_summary( pair ) :

    print('This is a dummy function') 



def plot_pair( pair , plot_path , show = True , tailind = 5000) :

    price1 = pair['Price1']
    price2 = pair['Price2']
    name1  = pair['Comp1']
    name2  = pair['Comp2']
    zscore = pair['z_scores']
    zscore_trade = pair['ZScoreTrading']
    zscore_open  = np.array( pair['ZScoreOpen']  )
    zscore_close = np.array( pair['ZScoreClose'] )
    possition = pair['Possition']
    possition_return = np.array( pair['PossitionReturn'] )
    plt.figure()
    plt.plot( ( price1[-tailind:] - np.nanmean(price1[-tailind:]) ) / np.nanstd( price1[-tailind:]) , 'b' ,label=name1)
    plt.plot( ( price2[-tailind:] - np.nanmean(price2[-tailind:]) ) / np.nanstd( price2[-tailind:]) , 'r' ,label=name2) 
    plt.legend()
    plt.title('Standarized prizes')
    plt.savefig( plot_path + '/Prices_' + name1 + '.' + name2 + '.png')

   
    plt.figure()
    plt.plot( zscore[-tailind:] , 'r' , label = 'Z-score' )
    plt.plot( zscore_trade[-tailind:] , 'b', label = 'Z-score trade')
    plt.legend()
    plt.title('Z-score')
    plt.savefig( plot_path + '/Zscore_'  + name1 + '.' + name2 + '.png')


    plt.figure()
    #tmp = np.copy( possition_return[-tailind:] )

    #tmp[tmp==0 ] = np.nan
    plt.plot( possition[-tailind:] , 'r' , label='Possition')
    #plt.plot( tmp , 'ok' , label='Possition return')
    plt.plot
    plt.legend()
    plt.title('Possition (+/-)')
    plt.savefig( plot_path + '/Possition_'  + name1 + '.' + name2 + '.png')


    plt.figure()
    plt.plot( np.cumprod( possition_return[-tailind:] + 1.0 ) , 'b' , label='Accumulated return')
    plt.legend()
    plt.title('Cumulative return (%)')
    plt.savefig( plot_path + '/PossitionRet_'  + name1 + '.' + name2 + '.png')

    plt.figure()
    plt.scatter( possition_return , zscore_open , c=zscore_close )
    plt.xlabel('Return %')
    plt.ylabel('Initial z-score')
    plt.savefig( plot_path + '/ScatterRet_'  + name1 + '.' + name2 + '.png')
    plt.colorbar()

    if show :
       plt.show()
   
