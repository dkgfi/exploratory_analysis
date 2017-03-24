import os
from sqlalchemy import create_engine
import pandas as pd
import matplotlib.pyplot as plt

password=os.getenv('pgpasswd', 'password')
engine_str='postgresql://willi:'+password+'@localhost:5432/gfi'
engine = create_engine(engine_str)

df_country = pd.read_sql_query('SELECT * FROM "country"', engine)
df_trade=pd.read_sql_query('SELECT month,ts,commodity_id, quantity_1,quantity_2, value FROM "trade" WHERE country_id=76 and src_country_id=83', engine)

loc=df_trade.loc[(df_trade['commodity_id']==29642)]
if 1:
    plt.figure();
    plt.plot(loc['month'],loc['value'])
    plt.show()
    
print('done')