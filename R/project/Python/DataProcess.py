import pandas as pd
import os

PATH = 'data'
names = ['R', 'Python']
for tp in ['confirmed', 'deaths']:
    #Pull
    urls = 'https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/{}-new.csv'.format(tp)
    data = pd.read_csv(urls, index_col=0)

    # Treatment
    regions = ['Brasil','Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste']
    df_states = data.drop(regions)
    df_regions = data.T[regions[1:]].T
    df_brasil = data.T[regions[0]].T

    # Save
    for name in names:
        try:
            URL = '{}/{}/{}/{}'.format('/'.join(os.getcwd().split('/')[:(-1)]), name, PATH, tp) 
            if not os.path.exists( URL ):
                os.makedirs( URL )
            df_states.to_csv(URL + '/data_states_new_{}.csv'.format(tp))
            df_regions.to_csv(URL + '/data_regions_new_{}.csv'.format(tp))
            df_brasil.to_csv(URL + '/data_brasil_new_{}.csv'.format(tp))
        except:
            URL = '{}/{}/{}/{}'.format('/'.join(os.getcwd().split('/')[:(-1)]), name, PATH, tp) 
            if not os.path.exists( URL ):
                os.makedirs( URL )
            df_states.to_csv(URL + '/data_states_new_{}.csv'.format(tp))
            df_regions.to_csv(URL + '/data_regions_new_{}.csv'.format(tp))
            df_brasil.to_csv(URL + '/data_brasil_new_{}.csv'.format(tp))