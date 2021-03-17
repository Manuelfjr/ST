import pandas as pd
import os

folder = 'data/'
names = ('ipeadata_icms.csv', 'ipeadata_igpdi.csv')

df_icms = pd.read_csv(folder + names[0], index_col=0)
df_igpdi = pd.read_csv(folder + names[1], index_col=0).drop(['Unnamed: 2'],axis =1 )
df_icms.columns, df_igpdi.columns = ['ICMS (RN)'], ['IGPDI']

a = []
for i in df_igpdi.index:
    if i in df_icms.index:
        a.append(True)
    else:
        a.append(False)
df_igpdi = df_igpdi[a]

df = df_igpdi
df['ICMS'] = df_icms['ICMS (RN)']

df.to_csv('/'.join(os.getcwd().split('/')[:(-1)]) + '/R/data/data_icms_igpdi.csv')
df.to_csv('data/data_icms_igpdi.csv')